const map = document.getElementById("map");
const highlight = document.getElementById("highlight");
const highlightContainer = document.getElementById("highlight-container");
const img = document.getElementById("img");
const canvas = document.getElementById("canvas");
const ctx = canvas.getContext("2d");

const percentEncode = url => {
    const pattern = /[:/?#[\]@!$&'()*+,;=%]/g;
    const encodeChar = c => "%" + c.charCodeAt(0).toString(16);
    return url.replace(pattern, encodeChar);
};

const svgUrl = svg => "data:image/svg+xml," + percentEncode(svg.outerHTML);

const loadImg = src => new Promise(resolve => {
    img.onload = () => resolve();
    img.src = src;
});

const draw = () => {
    ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
};

loadImg(svgUrl(map)).then(() => {
    draw();

    const elm = Elm.Main.init({
        node: document.getElementById("elm"),
        flags: canvas.toDataURL(),
    });

    const updateHighlightTexture = () => {
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        loadImg(svgUrl(highlight)).then(() => {
            draw();
            elm.ports.highlightTextureUrl.send(canvas.toDataURL());
        });
    };
    
    elm.ports.fillCountryByCode.subscribe(countryCode => {
        const filledCountry = document.getElementById(countryCode)
            .cloneNode(true);
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
});
