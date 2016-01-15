'use strict';

import React from 'react';
import Actions from '../actions/Actions';
import StudentStore from '../stores/StudentStore';

export default class Students extends React.Component {
    constructor() {
        super();
        Actions.loadStudents();
        this.state = {students: []};
    }

    componentDidMount() {
        this.unsubscribe = StudentStore.listen(this.onStudentsLoaded);
    }

    componentWillUnmount() {
        this.unsubscribe();
    }

    onStudentsLoaded(data) {
        this.setState({students: data});
    }

    render() {

    }
}
