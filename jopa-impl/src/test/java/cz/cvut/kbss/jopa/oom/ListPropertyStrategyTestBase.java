/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.mockito.Mock;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

class ListPropertyStrategyTestBase {

    protected static final URI PK = Generators.createIndividualIdentifier();

    @Mock
    protected ObjectOntologyMapperImpl mapperMock;

    protected MetamodelMocks mocks;
    protected Descriptor descriptor;

    AxiomValueGatherer builder;

    protected void setUp() throws Exception {
        this.mocks = new MetamodelMocks();
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(mocks.forOwlClassA().entityType());
        this.descriptor = new EntityDescriptor();
        this.builder =
                spy(new AxiomValueGatherer(NamedResource.create(PK), descriptor.getSingleContext().orElse(null)));
        when(mapperMock.containsEntity(any(), any(), any())).thenReturn(true);
    }

    static List<OWLClassA> generateList() {
        final List<OWLClassA> lst = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA_" + i));
            lst.add(a);
        }
        return lst;
    }

    static List<URI> generateListOfIdentifiers() {
        return generateList().stream().map(OWLClassA::getUri).collect(Collectors.toList());
    }

    void setRandomListItemsToNull(List<?> lst) {
        for (int i = 0; i < lst.size(); i++) {
            if (Generators.randomBoolean()) {
                lst.set(i, null);
            }
        }
    }

    void verifyListItems(List<URI> expected, ListValueDescriptor actual) {
        assertEquals(expected.size(), actual.getValues().size());
        for (int i = 0; i < expected.size(); i++) {
            assertEquals(expected.get(i), actual.getValues().get(i).getIdentifier());
        }
    }
}
