'use strict';

import React from 'react';
import ReactDOM from 'react-dom';
import Students from './components/Students';

/**
 * This is the application's entry point.
 *
 * The application is written using the new ES6 syntax (for the most part).
 */
class App extends React.Component {
    constructor() {
        super();
    }

    render() {
        return (<div>
            <h1>Example04 - JOPA + Spring</h1>
            <Students />
        </div>);
    }
}

ReactDOM.render(<App/>, document.getElementById('content'));
