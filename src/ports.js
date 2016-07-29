main.ports.setStorage.subscribe(function(state) {
    console.log('setting state', state);
    localStorage.setItem('elm-main-state', JSON.stringify(state));
});


main.ports.loadImage.subscribe(function(params) {
    console.log('LOADING IMAGE');
    console.log(params.url);
    console.log(params.authToken);
    console.log(params.fileId);

    var xhr = new XMLHttpRequest();
    xhr.open('GET', params.url, true);

    xhr.setRequestHeader('Authorization', 'Bearer ' + params.authToken);
    // Must include this line - specifies the response type we want
    xhr.responseType = 'arraybuffer';

    xhr.onload = function(e) {
        var arr = new Uint8Array(this.response);

        var raw = '';
        var i,j,subArray,chunk = 5000;
        for (i=0, j=arr.length; i<j; i+=chunk) {
           subArray = arr.subarray(i,i+chunk);
           raw += String.fromCharCode.apply(null, subArray);
        }

        // This works!!!
        var b64 = btoa(raw);
        var dataURL="data:image/jpeg;base64," + b64;

        main.ports.imageLoaded.send({
            fileId: params.fileId,
            imageData: b64
        });

        document.getElementById("image").src = dataURL;
    };

    xhr.send();
});
