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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.model.IRI;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class IdentifiableEntityTypeTest {

    private static Class<OWLClassA> cls;
    private static IRI classIri;
    private static String className;

    @BeforeAll
    static void setUpBeforeClass() {
        cls = OWLClassA.class;
        classIri = IRI.create(OWLClassA.getClassIri());
        className = OWLClassA.class.getName();
    }

    @Test
    void getAttributeThrowsIAEWhenAttributeIsNotPresent() {
        final EntityType<OWLClassA> et = new ConcreteEntityType<>(className, cls, classIri);
        assertThrows(IllegalArgumentException.class, () -> et.getAttribute("someUnknownAttribute"));
    }

    @Test
    void getDeclaredAttributeThrowsIAEWhenAttributeIsNotPresent() {
        final EntityType<OWLClassA> et = new ConcreteEntityType<>(className, cls, classIri);
        assertThrows(IllegalArgumentException.class, () -> et.getDeclaredAttribute("someUnknownAttribute"));
    }

    @Test
    void getFieldSpecificationThrowsIAEWhenAttributeIsNotPresent() {
        final EntityType<OWLClassA> et = new ConcreteEntityType<>(className, cls, classIri);
        assertThrows(IllegalArgumentException.class, () -> et.getFieldSpecification("someUnknownAttribute"));
    }

    @Test
    void getFieldSpecificationReturnsTypesIfNameMatches() throws Exception {
        final IdentifiableEntityType<OWLClassA> et = new ConcreteEntityType<>(className, cls, classIri);
        final TypesSpecification typesSpec = mock(TypesSpecification.class);
        when(typesSpec.getName()).thenReturn(OWLClassA.getTypesField().getName());
        when(typesSpec.getJavaField()).thenReturn(OWLClassA.getTypesField());
        et.addDirectTypes(typesSpec);

        assertEquals(typesSpec, et.getFieldSpecification(typesSpec.getName()));
    }
}
