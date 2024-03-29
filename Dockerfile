# Build stage 0
FROM erlang:23 as base

# elixir expects utf8.
ENV ELIXIR_VERSION="v1.9.4" \
	LANG=C.UTF-8

RUN set -xe \
	&& ELIXIR_DOWNLOAD_URL="https://github.com/elixir-lang/elixir/archive/${ELIXIR_VERSION}.tar.gz" \
	&& ELIXIR_DOWNLOAD_SHA256="f3465d8a8e386f3e74831bf9594ee39e6dfde6aa430fe9260844cfe46aa10139" \
	&& curl -fSL -o elixir-src.tar.gz $ELIXIR_DOWNLOAD_URL \
	&& echo "$ELIXIR_DOWNLOAD_SHA256  elixir-src.tar.gz" | sha256sum -c - \
	&& mkdir -p /usr/local/src/elixir \
	&& tar -xzC /usr/local/src/elixir --strip-components=1 -f elixir-src.tar.gz \
	&& rm elixir-src.tar.gz \
	&& cd /usr/local/src/elixir \
	&& make install clean

CMD ["iex"]

#Set working directory
RUN mkdir /data
WORKDIR /data
COPY rebar.config .
COPY rebar.lock .
COPY rebar3 .
RUN mix do local.hex --force, local.rebar --force

RUN elixir -v
RUN ./rebar3 --version

RUN ./rebar3 compile

FROM base as compile
COPY . .
COPY --from=base /data/_build .
#COPY src src/
#COPY priv priv/
#COPY config config/
COPY config/sys.config.local_docker config/sys.config
#COPY include include/
#COPY scripts scripts/
#COPY rebar.config .
#COPY rebar.lock .
#COPY rebar3 .

#Build the release
RUN ./rebar3 release

RUN pwd
RUN find .

#FROM alpine
FROM base as deploy

#RUN apk add openssl && \
#    apk add ncurses-libs && \
#    apk add libstdc++ && \
#    apk add libgcc 

RUN mkdir -p /opt/dog_trainer
RUN mkdir -p /var/log/dog_trainer
# Install the released application
COPY --from=compile /data/_build/default/rel/dog_trainer /opt/dog_trainer
#RUN sed -i 's/bin\/sh/bin\/sh -x/' /opt/dog_trainer/bin/dog_trainer
RUN ls -latr /var/log/dog_trainer

# Expose relevant ports
EXPOSE 7070

CMD ["/opt/dog_trainer/bin/dog_trainer", "foreground"]
