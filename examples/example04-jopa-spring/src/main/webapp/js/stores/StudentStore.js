'use strict';

var Reflux = require('reflux');
var request = require('superagent');

var Actions = require('../actions/Actions');

/**
 * Handles communication with the backend and listens to Actions invoked by the application.
 */
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
    },

    onSaveStudent: function (student) {
        request.post('rest/students').type('json').send(student).end(function (err) {
            if (err) {
                console.log(err);
            } else {
                this.onLoadStudents();
            }
        }.bind(this));
    },

    onDeleteStudent: function (student) {
        request.del('rest/students/' + student.key).end(function (err) {
            if (err) {
                console.log(err);
            } else {
                this.onLoadStudents();
            }
        }.bind(this));
    }
});

module.exports = StudentStore;
