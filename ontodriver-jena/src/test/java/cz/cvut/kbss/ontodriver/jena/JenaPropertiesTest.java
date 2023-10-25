/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.jena.util.Procedure;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class JenaPropertiesTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());

    @Mock
    private PropertiesHandler propertiesHandler;

    @Mock
    private Procedure beforeMock;

    @Mock
    private Procedure afterMock;

    private JenaProperties properties;

    @BeforeEach
    public void setUp() {
        final JenaAdapter adapterMock = mock(JenaAdapter.class);
        when(adapterMock.propertiesHandler()).thenReturn(propertiesHandler);
        this.properties = new JenaProperties(adapterMock, beforeMock, afterMock);
    }

    @Test
    public void getPropertiesInvokesPropertiesHandler() throws Exception {
        properties.getProperties(SUBJECT, null, false);
        verify(propertiesHandler).getProperties(SUBJECT, null, false);
    }

    @Test
    public void getPropertiesInvokesBeforeCallbackPriorToPropertiesLoading() throws Exception {
        properties.getProperties(SUBJECT, null, false);
        final InOrder inOrder = Mockito.inOrder(beforeMock, propertiesHandler);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(propertiesHandler).getProperties(SUBJECT, null, false);
    }

    @Test
    public void addPropertiesInvokesPropertiesHandler() throws Exception {
        final Map<Assertion, Set<Value<?>>> props = initValues();
        properties.addProperties(SUBJECT, null, props);
        verify(propertiesHandler).addProperties(SUBJECT, null, props);
    }

    private Map<Assertion, Set<Value<?>>> initValues() {
        final Assertion a = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        final Map<Assertion, Set<Value<?>>> props = new HashMap<>();
        props.put(a, Collections.singleton(new Value<>(117)));
        return props;
    }

    @Test
    public void addPropertiesInvokesCallbacks() throws Exception {
        final Map<Assertion, Set<Value<?>>> props = initValues();
        properties.addProperties(SUBJECT, null, props);
        final InOrder inOrder = Mockito.inOrder(beforeMock, propertiesHandler, afterMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(propertiesHandler).addProperties(SUBJECT, null, props);
        inOrder.verify(afterMock).execute();
    }

    @Test
    public void removePropertiesInvokesPropertiesHandler() throws Exception {
        final Map<Assertion, Set<Value<?>>> props = initValues();
        properties.removeProperties(SUBJECT, null, props);
        verify(propertiesHandler).removeProperties(SUBJECT, null, props);
    }

    @Test
    public void removePropertiesInvokesCallbacks() throws Exception {
        final Map<Assertion, Set<Value<?>>> props = initValues();
        properties.removeProperties(SUBJECT, null, props);
        final InOrder inOrder = Mockito.inOrder(beforeMock, propertiesHandler, afterMock);
        inOrder.verify(beforeMock).execute();
        inOrder.verify(propertiesHandler).removeProperties(SUBJECT, null, props);
        inOrder.verify(afterMock).execute();
    }
}
