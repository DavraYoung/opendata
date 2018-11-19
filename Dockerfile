FROM java:8-alpine

COPY target/uberjar/opendata.jar /opendata/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/opendata/app.jar"]
