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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassQ;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class EntityClassProcessorTest {

    @Test
    public void getEntityFieldsInEntityClassWithoutParentsFindsAllDeclaredFields() throws Exception {
        final Collection<Field> result = EntityClassProcessor.getEntityFields(OWLClassM.class);
        assertTrue(result.size() >= 7); // Can't use equals, because the AspectJ joint points are also returned
        assertTrue(result.contains(OWLClassM.getUriField()));
        assertTrue(result.contains(OWLClassM.getBooleanAttributeField()));
        assertTrue(result.contains(OWLClassM.getDateAttributeField()));
        assertTrue(result.contains(OWLClassM.getDoubleAttributeField()));
        assertTrue(result.contains(OWLClassM.getEnumAttributeField()));
        assertTrue(result.contains(OWLClassM.getIntAttributeField()));
        assertTrue(result.contains(OWLClassM.getLongAttributeField()));
    }

    @Test
    public void getEntityFieldsFindsFieldsInEntityAndMappedSuperclass() throws Exception {
        final Collection<Field> result = EntityClassProcessor.getEntityFields(OWLClassQ.class);
        assertTrue(result.size() >= 5); // Can't use equals, because the AspectJ joint points are also returned
        assertTrue(result.contains(OWLClassQ.getUriField()));
        assertTrue(result.contains(OWLClassQ.getLabelField()));
        assertTrue(result.contains(OWLClassQ.getOwlClassAField()));
        assertTrue(result.contains(OWLClassQ.getParentStringField()));
        assertTrue(result.contains(OWLClassQ.getStringAttributeField()));
    }

    @Test
    public void getEntityFieldsIgnoresNonEntitySuperclasses() throws Exception {
        final Collection<Field> result = EntityClassProcessor.getEntityFields(Child.class);
        assertTrue(result.contains(Child.class.getDeclaredField("uri")));
        assertFalse(result.contains(NonEntityParent.class.getDeclaredField("label")));
    }

    private static class NonEntityParent {
        private String label;
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ChildWithNonEntityParent")
    private static class Child extends NonEntityParent {
        @Id
        private URI uri;
    }
}
