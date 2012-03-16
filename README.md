Erltricity: An Erlang/OTP Thinkgear Connector Client 
==========

Developer: Ward Bekker <ward@equanimity.nl>

Description
----------

An Erlang/OTP Client Application for the [ThinkGear Socket Protocol](http://developer.neurosky.com/docs/lib/exe/fetch.php?media=app_notes:thinkgear_socket_protocol.pdf) from [NeuroSky](http://neurosky.com/). 

###You'll need one of [these](http://store.neurosky.com/products/mindwave-1):

![Fashion!](http://cdn.shopify.com/s/files/1/0031/6882/products/MindWave_large_medium.png)

###Software Requirements

* Erlang R15B+
* MySQL Server
* Thinkgear Connector (comes with installation media)
* An active internet connection as Google Graph API is used for the charting.

###Current development status

Code should be considered a proof of concept.

###Usage

Create a new MySQL database eg. `erltricity` and run the table creation statements from `table.sql`

Copy the database.config example file and adapt it to your MySQL Server needs:

`cp config/database.config.example config/database.config`

Start up an Erlang shell:

`rebar get-deps compile && ./shell.sh`

Now is a good time to turn on your Mindwave headset.

Inside Erlang shell:

`application:start(erltricity).`

And point your browser to [http://localhost:8000/chart](http://localhost:8000/chart) to see the recorded EGG signals. Manual refreshing is required in this version.

###Example Chart

With a proper signal, the chart should look something like this:

![Chart](https://img.skitch.com/20120316-f7yngmu28b5yq8xmusgabc13r3.jpg)
