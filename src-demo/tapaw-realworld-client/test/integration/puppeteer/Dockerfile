FROM buildkite/puppeteer:latest
COPY package.json /
RUN npm i
ENV PATH="${PATH}:/node_modules/.bin"
