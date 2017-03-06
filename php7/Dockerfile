FROM lablup/kernel-base:latest

# Install PHP 7.0
RUN apk add --no-cache --virtual .build-deps build-base zeromq-dev pkgconf autoconf automake \
    && apk add --no-cache php7 php7-dev php7-xml php7-pcntl php7-pear zeromq php7-iconv php7-zlib php7-json php7-gd php7-sqlite3 \
    # PECL patch (ref: http://stackoverflow.com/questions/40999752)
    && sed -i "$ s|\-n||g" /usr/bin/pecl \
    && echo '' | pecl install zmq-beta \
    && echo 'extension=zmq.so' >> /etc/php7/php.ini \
    && apk del .build-deps

ADD run.php /home/sorna/run.php
USER work
CMD ["/home/sorna/jail", "default", "/usr/bin/php7", "/home/sorna/run.php"]
