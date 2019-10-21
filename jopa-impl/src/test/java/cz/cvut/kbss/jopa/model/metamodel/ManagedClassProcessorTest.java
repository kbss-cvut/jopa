/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.QMappedSuperclass;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@SuppressWarnings("unused")
class ManagedClassProcessorTest {

    @Test
    void processManagedTypeThrowsInitializationExceptionWhenClassIsMissingNoArgConstructor() {
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> ManagedClassProcessor.processManagedType(ClassWithoutNoArgConstructor.class));
        assertEquals("Class " + ClassWithoutNoArgConstructor.class + " is missing required no-arg constructor.",
                ex.getMessage());
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ClassWithoutNoArgConstructor")
    private static class ClassWithoutNoArgConstructor {
        private String name;

        public ClassWithoutNoArgConstructor(String arg) {
            this.name = arg;
        }
    }

    @Test
    void processManagedTypeReturnsEntityTypeForEntity() {
        final TypeBuilderContext<OWLClassA> res = ManagedClassProcessor.processManagedType(OWLClassA.class);
        assertTrue(res.getType() instanceof EntityType);
    }

    @Test
    void processManagedTypeReturnsMappedSuperclassTypeForMappedSuperclass() {
        final TypeBuilderContext<QMappedSuperclass> res = ManagedClassProcessor
                .processManagedType(QMappedSuperclass.class);
        assertTrue(res.getType() instanceof MappedSuperclassType);
    }
}
