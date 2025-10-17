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
package cz.cvut.kbss.jopa.model.metamodel.gen;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassO;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.Manageable;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@Execution(ExecutionMode.SAME_THREAD)
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ManageableClassGeneratorTest {

    @Mock
    private UnitOfWork uow;

    private MetamodelMocks metamodelMocks;

    private final ManageableClassGenerator sut = new ManageableClassGenerator(new Configuration());

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        final MetamodelImpl mm = mock(MetamodelImpl.class);
        metamodelMocks.setMocks(mm);
        when(uow.getMetamodel()).thenReturn(mm);
    }

    @Test
    void generatedClassHasParentTypeAnnotations() {
        final Class<? extends OWLClassA> cls = sut.generate(OWLClassA.class);
        assertNotNull(cls.getAnnotation(OWLClass.class));
        assertEquals(OWLClassA.class.getAnnotation(OWLClass.class).iri(), cls.getAnnotation(OWLClass.class).iri());
    }

    @Test
    void generatedSetterInvokesPersistenceContextAttributeChangeHandler() throws Exception {
        final Class<? extends OWLClassA> cls = sut.generate(OWLClassA.class);

        final OWLClassA instance = cls.getDeclaredConstructor().newInstance();
        assertInstanceOf(Manageable.class, instance);
        assertInstanceOf(OWLClassA.class, instance);
        ((Manageable) instance).setPersistenceContext(uow);
        when(uow.isInTransaction()).thenReturn(true);
        assertEquals(uow, ((Manageable) instance).getPersistenceContext());
        instance.setStringAttribute("test value");
        assertEquals("test value", instance.getStringAttribute());
        verify(uow).attributeChanged(instance, metamodelMocks.forOwlClassA().stringAttribute());
    }

    @Test
    void doesNotOverrideSetterForTransientField() throws Exception {
        final Class<? extends OWLClassO> cls = sut.generate(OWLClassO.class);

        final OWLClassO instance = cls.getDeclaredConstructor().newInstance();
        ((Manageable) instance).setPersistenceContext(uow);
        when(uow.contains(instance)).thenReturn(true);

        instance.setTransientField("insignificant value");
        verify(uow, never()).attributeChanged(eq(instance), any(FieldSpecification.class));
        verify(uow, never()).attributeChanged(eq(instance), any(Field.class));
    }
}
