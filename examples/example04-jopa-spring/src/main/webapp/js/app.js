'use strict';

import React from 'react';
import ReactDOM from 'react-dom';

class App extends React.Component {
    constructor() {
        super();
    }

    render() {
        return <h1>Example04 - JOPA + Spring</h1>;
    }
}

ReactDOM.render(<App/>, document.getElementById('content'));
