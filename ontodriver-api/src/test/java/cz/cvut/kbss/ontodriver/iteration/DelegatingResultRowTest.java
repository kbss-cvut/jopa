/**
 * Copyright (C) 2019 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.iteration;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;

import static org.mockito.Mockito.verify;

class DelegatingResultRowTest {

    @Mock
    private ResultSet resultSet;

    private DelegatingResultRow sut;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
        this.sut = new DelegatingResultRow(resultSet);
    }

    @Test
    void getColumnCountDelegatesCallToResultSet() {
        sut.getColumnCount();
        verify(resultSet).getColumnCount();
    }

    @Test
    void findColumnDelegatesCallToResultSet() {
        sut.findColumn("test");
        verify(resultSet).findColumn("test");
    }

    @Test
    void isBoundByIndexDelegatesCallToResultSet() throws OntoDriverException {
        sut.isBound(117);
        verify(resultSet).isBound(117);
    }

    @Test
    void isBoundByNameDelegatesCallToResultSet() throws OntoDriverException {
        sut.isBound("test");
        verify(resultSet).isBound("test");
    }

    @Test
    void getBooleanByIndexDelegatesCallToResultSet() throws OntoDriverException {
        sut.getBoolean(1);
        verify(resultSet).getBoolean(1);
    }

    @Test
    void getBooleanByNameDelegatesCallToResultSet() throws OntoDriverException {
        sut.getBoolean("test");
        verify(resultSet).getBoolean("test");
    }

    @Test
    void getByteByIndexDelegatesCallToResultSet() throws OntoDriverException {
        sut.getByte(0);
        verify(resultSet).getByte(0);
    }

    @Test
    void getByteByNameDelegatesCallToResultSet() throws OntoDriverException {
        sut.getByte("test");
        verify(resultSet).getByte("test");
    }

    @Test
    void getDoubleByIndexDelegatesCallToResultSet() throws OntoDriverException {
        sut.getDouble(2);
        verify(resultSet).getDouble(2);
    }

    @Test
    void getDoubleByNameDelegatesCallToResultSet() throws OntoDriverException {
        sut.getDouble("test");
        verify(resultSet).getDouble("test");
    }

    @Test
    void getFloatByIndexDelegatesCallToResultSet() throws OntoDriverException {
        sut.getFloat(2);
        verify(resultSet).getFloat(2);
    }

    @Test
    void getFloatByNameDelegatesCallToResultSet() throws OntoDriverException {
        sut.getFloat("test");
        verify(resultSet).getFloat("test");
    }

    @Test
    void getIntByIndexDelegatesCallToResultSet() throws OntoDriverException {
        sut.getInt(2);
        verify(resultSet).getInt(2);
    }

    @Test
    void getIntByNameDelegatesCallToResultSet() throws OntoDriverException {
        sut.getInt("test");
        verify(resultSet).getInt("test");
    }

    @Test
    void getLongByIndexDelegatesCallToResultSet() throws OntoDriverException {
        sut.getLong(1);
        verify(resultSet).getLong(1);
    }

    @Test
    void getLongByNameDelegatesCallToResultSet() throws OntoDriverException {
        sut.getLong("test");
        verify(resultSet).getLong("test");
    }

    @Test
    void getObjectByIndexDelegatesCallToResultSet() throws OntoDriverException {
        sut.getObject(1);
        verify(resultSet).getObject(1);
    }

    @Test
    void getObjectByNameDelegatesCallToResultSet() throws OntoDriverException {
        sut.getObject("test");
        verify(resultSet).getObject("test");
    }

    @Test
    void getObjectByIndexAndClassDelegatesCallToResultSet() throws OntoDriverException {
        sut.getObject(1, URI.class);
        verify(resultSet).getObject(1, URI.class);
    }

    @Test
    void getObjectByNameAndClassDelegatesCallToResultSet() throws OntoDriverException {
        sut.getObject("test", URI.class);
        verify(resultSet).getObject("test", URI.class);
    }

    @Test
    void getShortByIndexDelegatesCallToResultSet() throws OntoDriverException {
        sut.getShort(1);
        verify(resultSet).getShort(1);
    }

    @Test
    void getShortByNameDelegatesCallToResultSet() throws OntoDriverException {
        sut.getShort("test");
        verify(resultSet).getShort("test");
    }

    @Test
    void getStringByIndexDelegatesCallToResultSet() throws OntoDriverException {
        sut.getString(1);
        verify(resultSet).getString(1);
    }

    @Test
    void getStringByNameDelegatesCallToResultSet() throws OntoDriverException {
        sut.getString("test");
        verify(resultSet).getString("test");
    }

    @Test
    void getIndexDelegatesCallToResultSet() throws OntoDriverException {
        sut.getIndex();
        verify(resultSet).getRowIndex();
    }
}