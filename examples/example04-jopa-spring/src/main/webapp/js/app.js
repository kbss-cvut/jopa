'use strict';

var React = require('react'),
    ReactDOM = require('react-dom');

var App = React.createClass({

    render: function () {
        return <h1>JOPA Example04</h1>;
    }
});

ReactDOM.render(<App/>, document.getElementById('content'));
