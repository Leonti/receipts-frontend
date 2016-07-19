FROM python:3.3.6

COPY css/style.min.css /root/css/style.min.css
COPY gen/main.js /root/gen/main.js
COPY index.html /root/index.html

WORKDIR /root

EXPOSE  8000

CMD python3 -m http.server
