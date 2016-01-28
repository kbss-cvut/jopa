'use strict';

import React from 'react';
import {Panel, Table} from 'react-bootstrap';
import Actions from '../actions/Actions';
import StudentRow from './StudentRow';
import AddStudent from './AddStudent';
import StudentStore from '../stores/StudentStore';

/**
 * Displays application UI - list of students and a form to add new ones.
 */
export default class Students extends React.Component {
    constructor() {
        super();
        Actions.loadStudents();
        this.state = {students: []};
    }

    componentDidMount() {
        this.unsubscribeStudents = StudentStore.listen(this.onStudentsLoaded.bind(this));
    }

    componentWillUnmount() {
        this.unsubscribeStudents();
    }

    onStudentsLoaded(data) {
        this.setState({students: data});
    }

    onDelete(student) {
        Actions.deleteStudent(student);
    }

    render() {
        return (
            <Panel header={<h3>Students</h3>} bsStyle='info'>
                <div className='col-xs-12'>
                    {this.renderStudents()}
                </div>
                <div className='col-xs-12'>
                    {this.renderAddForm()}
                </div>
            </Panel>
        );
    }

    renderStudents() {
        var students = this.state.students,
            len = students.length,
            rows = [];
        if (len === 0) {
            return null;
        }
        for (var i = 0; i < len; i++) {
            rows.push(<StudentRow key={'stud_' + i} student={students[i]} onDelete={this.onDelete}/>);
        }
        return (
            <div>
                <Table striped condensed hover bordered>
                    <thead>
                    <tr>
                        <td className='col-xs-6'>Name</td>
                        <td className='col-xs-5'>Email</td>
                        <td className='col-xs-1' style={{textAlign: 'center'}}>Actions</td>
                    </tr>
                    </thead>
                    <tbody>
                    {rows}
                    </tbody>
                </Table>
            </div>);
    }

    renderAddForm() {
        return (<AddStudent />);
    }
}
