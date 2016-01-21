'use strict';

import React from 'react';
import {Table} from 'react-bootstrap';
import Actions from '../actions/Actions';
import StudentStore from '../stores/StudentStore';
import StudentRow from './StudentRow';
import AddStudent from './AddStudent';

/**
 * This class acts as a controller for the whole UI.
 *
 * It downloads a list of students upon its mount and passes it on to the display components.
 * It also renders the form for adding new students.
 */
export default class Students extends React.Component {
    constructor() {
        super();
        Actions.loadStudents();
        this.state = {students: []};
    }

    componentDidMount() {
        this.unsubscribe = StudentStore.listen(this.onStudentsLoaded.bind(this));
    }

    componentWillUnmount() {
        this.unsubscribe();
    }

    onStudentsLoaded(data) {
        this.setState({students: data});
    }

    onDelete(student) {
        Actions.deleteStudent(student);
    }

    render() {
        return (<div>
            <div className='row col-xs-5'>
                {this.renderStudents()}
            </div>
            <div style={{clear: 'both'}}/>
            <div className='row col-xs-5'>
                {this.renderAddForm()}
            </div>
        </div>);
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
                <h3>Students</h3>
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
