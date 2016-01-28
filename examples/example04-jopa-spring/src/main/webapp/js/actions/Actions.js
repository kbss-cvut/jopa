'use strict';

var Reflux = require('reflux');

/**
 * Reflux actions, listened to by StudentStore.
 */
var Actions = Reflux.createActions([
    'loadStudents', 'saveStudent', 'deleteStudent', 'loadData'
]);

module.exports = Actions;
