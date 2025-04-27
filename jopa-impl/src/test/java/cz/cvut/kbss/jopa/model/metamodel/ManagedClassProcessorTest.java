/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.QMappedSuperclass;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.TestLocal;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.utils.ChangeTrackingMode;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.NamespaceResolver;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SuppressWarnings("unused")
class ManagedClassProcessorTest {

    @Test
    void processManagedTypeThrowsInitializationExceptionWhenClassIsMissingNoArgConstructor() {
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> ManagedClassProcessor.processManagedType(ClassWithoutNoArgConstructor.class, new NamespaceResolver(), new Configuration()));
        assertEquals("Entity " + ClassWithoutNoArgConstructor.class + " is missing required no-arg constructor.",
                ex.getMessage());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithoutNoArgConstructor")
    private static class ClassWithoutNoArgConstructor {
        private String name;

        public ClassWithoutNoArgConstructor(String arg) {
            this.name = arg;
        }
    }

    @Test
    void processManagedTypeReturnsEntityTypeForEntity() {
        final TypeBuilderContext<OWLClassA> res = ManagedClassProcessor.processManagedType(OWLClassA.class, new NamespaceResolver(), new Configuration());
        assertInstanceOf(EntityType.class, res.getType());
    }

    @Test
    void processManagedTypeGeneratesInstantiableTypeForEntityClass() {
        final TypeBuilderContext<OWLClassA> res = process(OWLClassA.class);
        final Class<? extends OWLClassA> instantiableType = res.getType().getInstantiableJavaType();
        assertTrue(OWLClassA.class.isAssignableFrom(instantiableType));
        assertNotEquals(OWLClassA.class, instantiableType);
    }

    private <T> TypeBuilderContext<T> process(Class<T> cls) {
        return ManagedClassProcessor.processManagedType(cls, new NamespaceResolver(), new Configuration());
    }

    @Test
    void processManagedTypeReturnsMappedSuperclassTypeForMappedSuperclass() {
        final TypeBuilderContext<QMappedSuperclass> res = process(QMappedSuperclass.class);
        assertInstanceOf(MappedSuperclassType.class, res.getType());
    }

    @Test
    void processManagedTypeReturnsAbstractEntityTypeTypeForInterfaces() {
        final TypeBuilderContext<InterfaceClass> res = process(InterfaceClass.class);
        assertInstanceOf(AbstractEntityType.class, res.getType());
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "Interface")
    private interface InterfaceClass {

    }

    @Test
    void processManagedTypeReturnsConcreteEntityTypeTypeForClasses() {
        final TypeBuilderContext<OWLEntity> res = process(OWLEntity.class);
        assertInstanceOf(ConcreteEntityType.class, res.getType());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "Class")
    public static class OWLEntity {

    }

    @Test
    void processManagedTypeThrowsExceptionOnNonManagedInterface() {
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> process(NonManagedInterfaceA.class));
        assertEquals("Type " + NonManagedInterfaceA.class + " is not a managed type.",
                ex.getMessage());
    }

    @Test
    void getManagedSuperInterfacesReturnsOnlyManagedInterfaces() {
        Set<Class<? super ChildClassWithMultipleParents>> managedSuperInterfaces = ManagedClassProcessor.getManagedSuperInterfaces(ChildClassWithMultipleParents.class);
        assertThat(managedSuperInterfaces, hasSize(2));
        assertThat(managedSuperInterfaces, hasItems(ManagedInterfaceA.class, ManagedInterfaceB.class));

    }

    @Test
    void getManagedSuperClassReturnsOnlyManagedSuperClass() {
        Class<? super ChildClassWithMultipleParents> managedSuperClass = ManagedClassProcessor.getManagedSuperClass(ChildClassWithMultipleParents.class);
        assertEquals(OWLEntity.class, managedSuperClass);
    }

    @Test
    void processManagedTypeCreatesEntityTypeWithEntityClassAsInstantiableTypeWhenOnCommitChangeTrackingModeIsConfigured() {
        final TypeBuilderContext<OWLClassA> result = ManagedClassProcessor.processManagedType(OWLClassA.class, new NamespaceResolver(), new Configuration(Map.of(JOPAPersistenceProperties.CHANGE_TRACKING_MODE, ChangeTrackingMode.ON_COMMIT.toString())));
        assertEquals(OWLClassA.class, result.getType().getInstantiableJavaType());
    }

    private interface NonManagedInterfaceA {

    }

    private interface NonManagedInterfaceB {

    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "InterfaceA")
    private interface ManagedInterfaceA {

    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "InterfaceB")
    private interface ManagedInterfaceB {

    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ChildClassWithMultipleParents")
    public static class ChildClassWithMultipleParents extends OWLEntity implements ManagedInterfaceA, ManagedInterfaceB, NonManagedInterfaceA, NonManagedInterfaceB {

    }
}
