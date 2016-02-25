/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.query.BindingSet;
import org.openrdf.query.TupleQueryResult;

import java.net.URI;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

public class SelectResultSetTest {

    private static final List<String> BINDINGS = Arrays.asList("x", "y", "z");

    @Mock
    private TupleQueryResult resultMock;
    @Mock
    private BindingSet bindingSetMock;
    @Mock
    private Statement statementMock;

    private ValueFactory valueFactory = new ValueFactoryImpl();

    private ResultSet resultSet;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(resultMock.getBindingNames()).thenReturn(BINDINGS);
        when(resultMock.next()).thenReturn(bindingSetMock);
        when(resultMock.hasNext()).thenReturn(true);

        this.resultSet = new SelectResultSet(resultMock, statementMock);
    }

    @Test
    public void getObjectOnLiteralReturnsCorrespondingJavaLiteral() throws Exception {
        final String x = "String";
        when(bindingSetMock.getValue("x")).thenReturn(valueFactory.createLiteral(x));
        final Boolean y = false;
        when(bindingSetMock.getValue("y")).thenReturn(valueFactory.createLiteral(y));
        final Integer z = 117;
        when(bindingSetMock.getValue("z")).thenReturn(valueFactory.createLiteral(z));

        resultSet.next();
        assertEquals(x, resultSet.getObject(0));
        assertEquals(y, resultSet.getObject("y"));
        assertEquals(z, resultSet.getObject(2));
    }

    @Test
    public void getObjectOnSesameUriReturnsJavaUri() throws Exception {
        final URI x = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#John117");
        when(bindingSetMock.getValue("x")).thenReturn(valueFactory.createURI(x.toString()));

        resultSet.next();
        assertEquals(x, resultSet.getObject(0));
    }

    @Test
    public void getObjectOnBlankNodeReturnsItAsString() throws Exception {
        when(bindingSetMock.getValue("x")).thenReturn(valueFactory.createBNode());

        resultSet.next();
        assertTrue(resultSet.getObject("x") instanceof String);
    }

    @Test
    public void getObjectTypedOnUriReturnsUriWhenAskedForUri() throws Exception {
        final URI x = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#John117");
        when(bindingSetMock.getValue("x")).thenReturn(valueFactory.createURI(x.toString()));

        resultSet.next();
        assertEquals(x, resultSet.getObject(0, URI.class));
    }

    @Test
    public void getObjectReturnsNullWhenOptionalFindsNoBinding() throws Exception {
        when(bindingSetMock.getValue("x")).thenReturn(null);

        resultSet.next();
        assertNull(resultSet.getObject("x"));
    }

    @Test
    public void getObjectTypedReturnsNullWhenOptionalFindsNoBinding() throws Exception {
        when(bindingSetMock.getValue("x")).thenReturn(null);

        resultSet.next();
        assertNull(resultSet.getObject("x", URI.class));
    }
}