'use strict';

import React from 'react';
import {Button} from 'react-bootstrap';
import assign from 'object-assign';

/**
 * Single row in the student list.
 */
export default class StudentRow extends React.Component {
    constructor(props) {
        super(props);
    }

    onDelete() {
        this.props.onDelete(this.props.student);
    }

    render() {
        var student = this.props.student,
            name = student.firstName + ' ' + student.lastName;
        return (
            <tr>
                <td>{name}</td>
                <td>{student.email}</td>
                <td><Button bsSize='small' bsStyle='warning' onClick={this.onDelete.bind(this)}>Delete</Button>
                </td>
            </tr>
        );
    }
}
