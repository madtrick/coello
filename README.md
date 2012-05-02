Coello
======

A thin wrapper over the RabbitMQ erlang libraries

* [Disclaimer](#disclaimer)
* [AMQP conformance](#conformance)
* [License](#licesne)

### Disclaimer <a name="disclaimer"> ###

This project is still a __work in progress__ and it's on beta state.

### AMQP conformance <a name="conformance"> ###

Currently the supported AMQP (version 0.9.1) classes and their respective methods are:

* Class connection
  * start
      * `coello_connection:start/0`, opens a connection using the default parameters.
      * `coello_connection:close/1`, closes a connection.
* Class basic
  * publish
      * `coello_basic:publish/4`, publish a message.
      * `coello_basic:publish/5`, publish a message but include _reply_to_ field.
  * consume
      * `coello_basic:consume/3`, consume messages. It starts a consumer process that'll invoke a callback on each new message.
  * cancel
      * `coello_basic:cancel/2`, cancels a consumer.
* Class channel
  * open
      * `coello_channel:open/1`, opens a channel.
  * close
      * `coello_channel:close/1`, closes a channel.
* Class exchange
  * declare
      * `coello_exchange:declare/3`, declares a non-durable exchange.
* Class queue
  * declare
      * `coello_queue:declare/1`, declares an exclusive, non-durable queue.
      * `coello_queue:declare/2`, declares an exclusive, non-durable queue, with a chosen name.
  * bind
      * `coello_queue:bind/4`, binds a queue.
  * delete
      * `coello_queue:delete/2`, deletes a queue.

### License <a name="license"> ###
Licensed under Apache 2.0. Check LICENSE for details
