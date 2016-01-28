'use strict';

import React from 'react';
import Data from './Data';
import Students from './Students';

export default class MainView extends React.Component {
    constructor() {
        super();
    }

    render() {
        return (
            <div>
                <div className='col-xs-5'>
                    <Students />
                </div>
                <div className='col-xs-7'>
                    <Data />
                </div>
            </div>
        );
    }
}
