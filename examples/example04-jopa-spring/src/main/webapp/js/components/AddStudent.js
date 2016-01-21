'use strict';

import React from 'react';
import {Button, Input, Well} from 'react-bootstrap';
import Actions from '../actions/Actions';

export default class AddStudent extends React.Component {
    constructor() {
        super();
        this.state = {}
    }

    onChange(e) {
        var change = {};
        change[e.target.name] = e.target.value;
        this.setState(change);
    }

    onSubmit() {
        Actions.saveStudent(this.state);
        this.setState({
            firstName: '',
            lastName: '',
            email: ''
        });
    }

    render() {
        return (
            <Well>
                <h3>Add Student</h3>
                <Input type='text' name='firstName' label='First name' onChange={this.onChange.bind(this)}
                       value={this.state.firstName}/>
                <Input type='text' name='lastName' label='Last name' onChange={this.onChange.bind(this)}
                       value={this.state.lastName}/>
                <Input type='text' name='email' label='Email' onChange={this.onChange.bind(this)} value={this.state.email}/>
                <Button onClick={this.onSubmit.bind(this)}>Submit</Button>
            </Well>
        )
    }
}
