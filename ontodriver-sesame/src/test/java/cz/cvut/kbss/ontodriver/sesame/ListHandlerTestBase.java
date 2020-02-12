/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.junit.Test;
import org.mockito.Mock;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class ListHandlerTestBase<D extends ListDescriptor, V extends ListValueDescriptor> {

    static final NamedResource OWNER = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/entityC");

    static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSequence";
    static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasNext";

    static IRI hasListProperty;
    static IRI nextNodeProperty;

    protected static ValueFactory vf;
    static Resource owner;

    @Mock
    Connector connector;

    D listDescriptor;
    V valueDescriptor;

    ListHandler<D, V> handler;

    static void init() {
        vf = SimpleValueFactory.getInstance();
        owner = vf.createIRI(OWNER.toString());
        hasListProperty = vf.createIRI(LIST_PROPERTY);
        nextNodeProperty = vf.createIRI(NEXT_NODE_PROPERTY);
    }

    static List<NamedResource> initList() {
        final List<NamedResource> lst = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            lst.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/elem" + i));
        }
        return lst;
    }

    @Test
    public void loadsEmptyListAndReturnsEmptyCollection() throws Exception {
        when(connector.findStatements(owner, hasListProperty, null, false)).thenReturn(Collections.emptyList());
        final Collection<Axiom<NamedResource>> res = handler.loadList(listDescriptor);
        assertNotNull(res);
        assertTrue(res.isEmpty());
        verify(connector, never()).findStatements(any(Resource.class), eq(nextNodeProperty),
                any(Value.class), any(Boolean.class), eq(null));
    }
}
