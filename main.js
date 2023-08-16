/* ---
 * initialize elm
 * ---
 */

const elm = Elm.Main.init({ node: document.getElementById("ui") });


/* ---
 * playing sounds
 * ---
 */

const correctSound = new Audio("correct.wav");
const wrongSound = new Audio("wrong.mp3");

const listeningForCorrect = new Promise(resolve => {
    correctSound.oncanplaythrough = () => {
        elm.ports.sendCorrect.subscribe(() => {
            correctSound.play();
        });
        resolve();
    };
    // needed on iphone safari
    correctSound.load();
});

const listeningForWrong = new Promise(resolve => {
    wrongSound.oncanplaythrough = () => {
        elm.ports.sendWrong.subscribe(() => {
            wrongSound.play();
        });
        resolve();
    };
    // needed on iphone safari
    wrongSound.load();
});


/* ---
 * get webgl context
 * ---
 */

const glCanvas = document.getElementById("webgl");
const gl = glCanvas.getContext("webgl");


/* ---
 * configure webgl
 * ---
 */

gl.enable(gl.DEPTH_TEST);


/* ---
 * set up shader program
 * ---
 */

const makeShader = (gl, type, source) => {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    const success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
    if (success) return shader;
    throw new Error(gl.getShaderInfoLog(shader));
};

const makeShaderProgram = (gl, vertexShader, fragmentShader) => {
    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    const success = gl.getProgramParameter(program, gl.LINK_STATUS);
    if (success) return program;
    throw new Error(gl.getProgramInfoLog(program));
};

const vertexShader = makeShader(gl, gl.VERTEX_SHADER, `
    attribute vec3 pos;
    attribute vec2 textureCoords;
    uniform mat4 rotation;
    uniform mat4 camera;
    uniform mat4 perspective;
    varying vec2 vTextureCoords;
    varying vec3 vPos;
    void main() {
        gl_Position = perspective * camera * rotation * vec4(pos, 1.0);
        vTextureCoords = textureCoords;
        vPos = mat3(rotation) * pos;
    }
`);

const fragmentShader = makeShader(gl, gl.FRAGMENT_SHADER, `
    precision lowp float;
    uniform sampler2D mapTexture;
    uniform sampler2D highlightTexture;
    uniform float ambientBrightness;
    uniform float diffuseBrightness;
    uniform float specularBrightness;
    uniform vec3 lightColor;
    uniform vec3 lightDir;
    uniform vec3 cameraPos;
    uniform float shininess;
    varying vec2 vTextureCoords;
    varying vec3 vPos;

    vec4 sampleTextures() {
        vec4 mapColor = texture2D(mapTexture, vTextureCoords);
        vec4 highlightColor = texture2D(highlightTexture, vTextureCoords);
        float alpha = highlightColor.w + mapColor.w * (1.0 - highlightColor.w);
        vec3 rgb =
            (highlightColor.xyz * highlightColor.w
                + mapColor.xyz * mapColor.w * (1.0 - highlightColor.w))
            / alpha;
        return vec4(rgb, alpha);
    }

    void main() {
        vec3 ambient = ambientBrightness * lightColor;
        // normal is the same as position since unit sphere centred at origin
        vec3 normal = vPos;
        vec3 diffuse = diffuseBrightness
            * max(dot(normal, lightDir), 0.0)
            * lightColor;
        vec3 cameraDir = normalize(cameraPos - vPos);
        vec3 reflectDir = reflect(-lightDir, normal);
        vec3 specular = specularBrightness
            * pow(max(dot(cameraDir, reflectDir), 0.0), shininess)
            * lightColor;
        vec3 lighting = ambient + diffuse + specular;
        vec4 objColor = sampleTextures();
        gl_FragColor = vec4(lighting, 1.0) * objColor;
    }
`);

const shaderProgram = makeShaderProgram(gl, vertexShader, fragmentShader);
gl.useProgram(shaderProgram);


/* ---
 * loading the mesh
 * ---
 */

const locateAttr = (gl, program, attrName) => {
    const attr = gl.getAttribLocation(program, attrName);
    if (attr === -1) {
        throw new Error(`could not locate attribute "${attrName}"`);
    }
    return attr;
};

const configureAttr = (gl, attr, { size, type, normalize, stride,
        offset }) => {
    gl.vertexAttribPointer(attr, size, type, normalize, stride, offset);
};

const meshReceived = new Promise(resolve => {
    elm.ports.sendMesh.subscribe(resolve);
});

const meshLoaded = meshReceived.then(mesh => {
    const pos = locateAttr(gl, shaderProgram, "pos");
    gl.enableVertexAttribArray(pos);

    const posBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, posBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(mesh.pos), gl.STATIC_DRAW);
    configureAttr(gl, pos, { size: 3, type: gl.FLOAT, normalize: false,
        stride: 0, offset: 0 });

    const textureCoords = locateAttr(gl, shaderProgram, "textureCoords");
    gl.enableVertexAttribArray(textureCoords);

    const textureCoordsBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, textureCoordsBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(mesh.textureCoords),
        gl.STATIC_DRAW);
    configureAttr(gl, textureCoords, { size: 2, type: gl.FLOAT,
        normalize: false, stride: 0, offset: 0 });

    return mesh.numVertices;
});


/* ---
 * setting canvas dimensions
 * ---
 */

const glCanvasStyle = document.createElement("style");
document.head.appendChild(glCanvasStyle);

const setGlDimensions = (width, height) => {
    glCanvas.width = 2 * width;
    glCanvas.height = 2 * height;
    gl.viewport(0, 0, 2 * width, 2 * height);
    glCanvasStyle.innerText = `
        #webgl {
            width: ${width}px;
            height: ${height}px;
            display: block;
            background-color: #2e4482;
        }
    `;
};


/* ---
 * setting uniforms
 * ---
 */

const locateUniform = (gl, program, uniformName) => {
    const uniform = gl.getUniformLocation(program, uniformName);
    if (uniform === null) {
        throw new Error(`could not locate uniform "${uniformName}"`);
    }
    return uniform;
};

const rotation = locateUniform(gl, shaderProgram, "rotation");
const camera = locateUniform(gl, shaderProgram, "camera");
const perspective = locateUniform(gl, shaderProgram, "perspective");
const ambientBrightness =
    locateUniform(gl, shaderProgram, "ambientBrightness");
const diffuseBrightness =
    locateUniform(gl, shaderProgram, "diffuseBrightness");
const specularBrightness =
    locateUniform(gl, shaderProgram, "specularBrightness");
const lightColor = locateUniform(gl, shaderProgram, "lightColor");
const lightDir = locateUniform(gl, shaderProgram, "lightDir");
const cameraPos = locateUniform(gl, shaderProgram, "cameraPos");
const shininess = locateUniform(gl, shaderProgram, "shininess");

const setUniforms = uniforms => {
    gl.uniformMatrix4fv(rotation, false, new Float32Array(uniforms.rotation));
    gl.uniformMatrix4fv(camera, false, new Float32Array(uniforms.camera));
    gl.uniformMatrix4fv(perspective, false,
        new Float32Array(uniforms.perspective));
    gl.uniform1f(ambientBrightness, uniforms.ambientBrightness);
    gl.uniform1f(diffuseBrightness, uniforms.diffuseBrightness);
    gl.uniform1f(specularBrightness, uniforms.specularBrightness);
    gl.uniform3fv(lightColor, new Float32Array(uniforms.lightColor));
    gl.uniform3fv(lightDir, new Float32Array(uniforms.lightDir));
    gl.uniform3fv(cameraPos, new Float32Array(uniforms.cameraPos));
    gl.uniform1f(shininess, uniforms.shininess);
};


/* ---
 * re-rendering when we receive render update
 * ---
 */

const listeningForRenderUpdate = meshLoaded.then(numVertices => {
    elm.ports.sendRenderUpdate.subscribe(update => {
        if (update.hasOwnProperty("windowDimensions")) {
            // update canvas to match window dimensions
            const { width, height } = update.windowDimensions;
            setGlDimensions(width, height);
        }

        if (update.hasOwnProperty("uniforms")) {
            setUniforms(update.uniforms);
        }

        // re-draw
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        gl.drawArrays(gl.TRIANGLES, 0, numVertices);
    });
});


/* ---
 * loading the textures
 * ---
 */

const percentEncode = url => {
    const pattern = /[:/?#[\]@!$&'()*+,;=%]/g;
    const encodeChar = c => "%" + c.charCodeAt(0).toString(16);
    return url.replace(pattern, encodeChar);
};

const svgUrl = svg => "data:image/svg+xml," + percentEncode(svg.outerHTML);

const loadSvgImage = svg => new Promise(resolve => {
    const image = new Image();
    image.onload = () => resolve(image);
    image.src = svgUrl(svg);
});

// loads image into the current active texture
const loadTexture = (gl, image) => {
    const level = 0;
    const internalFormat = gl.RGBA;
    const srcFormat = gl.RGBA;
    const srcType = gl.UNSIGNED_BYTE;
    gl.texImage2D(gl.TEXTURE_2D, level, internalFormat, srcFormat, srcType,
        image);
    // can't use mipmap since too much distortion near the poles
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
};

const map = document.getElementById("map");
const highlight = document.getElementById("highlight");
const highlightContainer = document.getElementById("highlight-container");

const svgWidth = 8192;
const svgHeight = 4096;

map.setAttribute("width", svgWidth);
map.setAttribute("height", svgHeight);
highlight.setAttribute("width", svgWidth);
highlight.setAttribute("height", svgHeight);

// use texture 0 for map and texture 1 for highlight
const mapTexture = locateUniform(gl, shaderProgram, "mapTexture");
gl.uniform1i(mapTexture, 0);
const highlightTexture = locateUniform(gl, shaderProgram, "highlightTexture");
gl.uniform1i(highlightTexture, 1);

const mapTextureLoaded = loadSvgImage(map).then(mapImage => {
    gl.activeTexture(gl.TEXTURE0);
    const texture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, texture);
    loadTexture(gl, mapImage);
});

// initialize highlight texture
{
    gl.activeTexture(gl.TEXTURE1);
    const texture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, texture);
}

const updateHighlightTexture = () => {
    const highlightTextureLoaded = loadSvgImage(highlight)
            .then(highlightImage => {
        gl.activeTexture(gl.TEXTURE1);
        loadTexture(gl, highlightImage);
    });

    Promise.all([meshLoaded, mapTextureLoaded, highlightTextureLoaded,
            listeningForRenderUpdate, listeningForCorrect,
            listeningForWrong]).then(() => {
        elm.ports.jsReadySignal.send(null);
    });
};

elm.ports.fillCountryByCode.subscribe(countryCode => {
    const filledCountry = document.getElementById(countryCode).cloneNode(true);
    filledCountry.removeAttribute("id");
    highlightContainer.replaceChildren(filledCountry);
    updateHighlightTexture();
});

// doesn't work if coords are too close to the poles, but luckily no
// country is that close to the poles
elm.ports.drawDots.subscribe(dots => {
    highlightContainer.replaceChildren();
    for (const { latitude, longitude, radius } of dots) {
        const squeezeFactor = Math.cos(latitude / 180 * Math.PI);
        const dot = document.createElementNS("http://www.w3.org/2000/svg",
            "ellipse");
        dot.setAttribute("cx", 180 + longitude);
        dot.setAttribute("cy", 90 - latitude);
        dot.setAttribute("rx", radius / squeezeFactor);
        dot.setAttribute("ry", radius);
        highlightContainer.appendChild(dot);
    }
    updateHighlightTexture();
});
