/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import org.mockito.Mock;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.when;

public class ListPropertyStrategyTestBase {

    protected static final URI PK = URI
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/entityC");

    @Mock
    protected EntityMappingHelper mapperMock;

    @Mock
    protected CascadeResolver cascadeResolverMock;

    protected MetamodelMocks mocks;
    protected Descriptor descriptor;

    protected void setUp() throws Exception {
        this.mocks = new MetamodelMocks();
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(mocks.forOwlClassA().entityType());
        this.descriptor = new EntityDescriptor();
    }

    protected static List<OWLClassA> generateList() {
        final List<OWLClassA> lst = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA_"
                            + i));
            lst.add(a);
        }
        return lst;
    }
}
