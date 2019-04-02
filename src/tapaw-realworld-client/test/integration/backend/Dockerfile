FROM node:8-alpine

RUN apk add git
RUN git clone https://github.com/gothinkster/node-express-realworld-example-app
WORKDIR /node-express-realworld-example-app
RUN npm install

EXPOSE 3000
ENTRYPOINT ["npm", "run", "start"]
