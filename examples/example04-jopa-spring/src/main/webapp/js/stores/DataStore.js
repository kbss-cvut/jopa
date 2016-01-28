'use strict';

var Reflux = require('reflux');
var request = require('superagent');

var Actions = require('../actions/Actions');

/**
 * Handles raw repository data loading.
 */
var DataStore = Reflux.createStore({
    listenables: [Actions],

    onLoadData: function (format) {
        request.get('rest/data?format=' + format).accept('text/plain').end(function (err, resp) {
            if (err) {
                console.log(err);
            } else {
                this.trigger(resp.text);
            }
        }.bind(this));
    }
});

module.exports = DataStore;
