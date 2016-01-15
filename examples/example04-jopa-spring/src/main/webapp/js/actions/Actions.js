'use strict';

var Reflux = require('reflux');

var Actions = Reflux.createActions([
    'loadStudents', 'saveStudent', 'deleteStudent'
]);

module.exports = Actions;
