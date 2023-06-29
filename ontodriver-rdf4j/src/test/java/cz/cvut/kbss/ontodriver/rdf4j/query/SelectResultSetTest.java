/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.VariableNotBoundException;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SelectResultSetTest {

    private static final List<String> BINDINGS = Arrays.asList("x", "y", "z");

    @Mock
    private TupleQueryResult resultMock;
    @Mock
    private BindingSet bindingSetMock;
    @Mock
    private Statement statementMock;

    private final ValueFactory valueFactory = SimpleValueFactory.getInstance();

    private ResultSet resultSet;

    @BeforeEach
    public void setUp() {
        when(resultMock.getBindingNames()).thenReturn(BINDINGS);
        when(resultMock.next()).thenReturn(bindingSetMock);
        when(resultMock.hasNext()).thenReturn(true);

        this.resultSet = new SelectResultSet(resultMock, statementMock);
    }

    @Test
    public void getObjectOnLiteralReturnsCorrespondingJavaLiteral() throws Exception {
        final String x = "String";
        when(bindingSetMock.getValue("x")).thenReturn(valueFactory.createLiteral(x));
        when(bindingSetMock.getValue("y")).thenReturn(valueFactory.createLiteral(false));
        final int z = 117;
        when(bindingSetMock.getValue("z")).thenReturn(valueFactory.createLiteral(z));

        resultSet.next();
        assertEquals(x, resultSet.getObject(0));
        assertFalse((Boolean) resultSet.getObject("y"));
        assertEquals(z, resultSet.getObject(2));
    }

    @Test
    public void getObjectOnRdf4jIriReturnsJavaUri() throws Exception {
        final URI x = Generator.generateUri();
        when(bindingSetMock.getValue("x")).thenReturn(valueFactory.createIRI(x.toString()));

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
        final URI x = URI.create("https://onto.fel.cvut.cz/ontologies/jopa#John117");
        when(bindingSetMock.getValue("x")).thenReturn(valueFactory.createIRI(x.toString()));

        resultSet.next();
        assertEquals(x, resultSet.getObject(0, URI.class));
    }

    @Test
    public void isBoundReturnsTrueForBoundVariableIndex() throws Exception {
        when(bindingSetMock.getBindingNames()).thenReturn(Collections.singleton("x"));
        when(bindingSetMock.getValue("x")).thenReturn(valueFactory.createLiteral(117));
        resultSet.next();
        assertTrue(resultSet.isBound(0));
    }

    @Test
    public void isBoundReturnsFalseForUnboundVariableIndex() throws Exception {
        when(bindingSetMock.getBindingNames()).thenReturn(Collections.singleton("x"));
        when(bindingSetMock.getValue("x")).thenReturn(null);
        resultSet.next();
        assertFalse(resultSet.isBound(0));
    }

    @Test
    public void isBoundReturnsTrueForBoundVariableName() throws Exception {
        when(bindingSetMock.getBindingNames()).thenReturn(Collections.singleton("x"));
        when(bindingSetMock.getValue("x")).thenReturn(valueFactory.createLiteral(117));
        resultSet.next();
        assertTrue(resultSet.isBound("x"));
    }

    @Test
    public void isBoundReturnsFalseForUnboundVariableName() throws Exception {
        when(bindingSetMock.getBindingNames()).thenReturn(Collections.singleton("x"));
        when(bindingSetMock.getValue("x")).thenReturn(null);
        resultSet.next();
        assertFalse(resultSet.isBound("x"));
    }

    @Test
    public void getObjectThrowsVariableNotBoundWhenVariableIsInResultSetButIsNotBoundInRow() throws Exception {
        when(bindingSetMock.getBindingNames()).thenReturn(new HashSet<>(Arrays.asList("x", "y")));
        when(bindingSetMock.getValue("x")).thenReturn(valueFactory.createLiteral(117));
        resultSet.next();
        assertThrows(VariableNotBoundException.class, () -> resultSet.getObject("y"));
    }

    @Test
    public void getIntByIndexThrowsVariableNotBoundWhenVariableIsInResultSetButIsNotBoundInRow() throws Exception {
        when(bindingSetMock.getBindingNames()).thenReturn(new HashSet<>(Arrays.asList("x", "y")));
        when(bindingSetMock.getValue("x")).thenReturn(valueFactory.createLiteral(117));
        resultSet.next();
        assertThrows(VariableNotBoundException.class, () -> resultSet.getInt(1));
    }
}
