'use strict';

var Reflux = require('reflux');
var request = require('superagent');

var Actions = require('../actions/Actions');

var StudentStore = Reflux.createStore({
    listenables: [Actions],

    onLoadStudents: function () {
        request.get('rest/students').accept('json').end(function (err, resp) {
            if (err) {
                console.log(err);
            } else {
                this.trigger(resp.body);
            }
        }.bind(this));
    }
});

module.exports = StudentStore;
