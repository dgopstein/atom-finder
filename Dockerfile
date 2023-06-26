FROM clojure
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
COPY project.clj /usr/src/app/
RUN lein deps
COPY . /usr/src/app

RUN mkdir -p /usr/src/app/repos

RUN git clone https://github.com/nginx/nginx.git /usr/src/app/repos/nginx
RUN git -C /usr/src/app/repos/nginx checkout 9cb9ce7

WORKDIR /usr/src/app
CMD ["lein", "run", "repos/nginx"]
