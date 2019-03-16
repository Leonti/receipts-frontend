FROM nginx:alpine

COPY css/style.min.css /usr/share/nginx/html/css/style.min.css
COPY gen/main.js /usr/share/nginx/html/gen/main.js
COPY src/auth-ports.js /usr/share/nginx/html/src/auth-ports.js
COPY src/ports.js /usr/share/nginx/html/src/ports.js
COPY index.html /usr/share/nginx/html/index.html
