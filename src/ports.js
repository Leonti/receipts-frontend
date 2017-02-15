main.ports.setStorage.subscribe(function(state) {
//    console.log('setting state', state);
    localStorage.setItem('elm-main-state', JSON.stringify(state));
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



main.ports.createReceipt.subscribe(function(params) {

    console.log('Creating receipt', params);

    var file = document.getElementById(params.fileInputId).files[0];
    var formData = new FormData();
    formData.append('total', params.receiptDetails.total);
    formData.append('description', params.receiptDetails.description);
    formData.append('receipt', file);

    var xhr = new XMLHttpRequest();

    xhr.upload.addEventListener('progress', function(event) {
      var percentComplete = event.loaded / event.total;
      console.log('LOADED: ', percentComplete);
    });

    xhr.upload.addEventListener("load", function(event) {
        console.log('TRANSFER COMPLETE', event);
        main.ports.receiptCreated.send({
            receiptId: 'receiptId',
            error: null
        });
    });

    xhr.upload.addEventListener("error", function(event) {
        console.log('TRANSFER ERROR');
        main.ports.receiptCreated.send({
            receiptId: null,
            error: 'Error uploading'
        });
    });

    xhr.open('POST', params.url);
    xhr.setRequestHeader('Authorization', 'Bearer ' + params.authToken);

    xhr.send(formData);
});
