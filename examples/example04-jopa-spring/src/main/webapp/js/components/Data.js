'use strict';

import React from 'react';
import {Panel, Input} from 'react-bootstrap';

import DataStore from '../stores/DataStore';
import StudentStore from '../stores/StudentStore';
import Actions from '../actions/Actions';

/**
 * Displays raw data from the storage.
 */
export default class Data extends React.Component {
    constructor() {
        super();
        this.state = {data: '', format: 'rdfxml'}
    }

    componentDidMount() {
        this.unsubscribe = DataStore.listen(this.onDataLoaded.bind(this));
        this.unsubscribeStudents = StudentStore.listen(this.onLoadData.bind(this));
    }

    componentWillUnmount() {
        this.unsubscribe();
        this.unsubscribeStudents();
    }

    onDataLoaded(data) {
        this.setState({data: data});
    }

    onLoadData() {
        Actions.loadData(this.state.format);
    }

    onFormatSelected(e) {
        var format = e.target.value;
        this.setState({format: format});
        Actions.loadData(format);
    }


    render() {
        return (
            <Panel header={<h3>Repository Content</h3>} bsStyle='info' style={{height: '100%'}}>
                <Input type='select' value={this.state.format} onChange={this.onFormatSelected.bind(this)}>
                    <option value='rdfxml'>RDF/XML (Pretty)</option>
                    <option value='json'>JSON</option>
                    <option value='turtle'>Turtle</option>
                </Input>
                <Input type='textarea' rows='20' value={this.state.data}/>
            </Panel>
        );
    }
}
