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

main.ports.initDownload.subscribe(function(url) {
    window.location = url;
});

var fileListeners = {};

main.ports.receiptFileMouseDown.subscribe(function(id) {
    console.log('Subscribing to', id);

    if (!fileListeners[id]) {
        fileListeners[id] = true;
        document.getElementById(id).onchange = function(event) {
            var files = event.target.files;
            console.log('file selected', files);

            if (files.length > 0) {
                var file = files[0]

                var imageType = /^image\//;
                if (imageType.test(file.type)) {
                    var reader = new FileReader();
                    reader.onload = function(e) {
                        console.log('Image result');
                        main.ports.receiptFileSelected.send({
                            isImage: true,
                            imageDataUrl: e.target.result
                        });
                    }
                    reader.readAsDataURL(files[0]);
                } else {
                    main.ports.receiptFileSelected.send({
                        isImage: false,
                        imageDataUrl: null
                    });
                }

            }
        };
    }


});

/*
document.getElementById('receipt-file').onchange = function() {
    console.log('file selected');
    document.getElementById("test-button").disabled = false
};

*/
