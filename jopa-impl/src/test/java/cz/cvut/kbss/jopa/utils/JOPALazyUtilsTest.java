/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingListProxy;
import cz.cvut.kbss.jopa.proxy.lazy.gen.LazyLoadingEntityProxyGenerator;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class JOPALazyUtilsTest {

    @Mock
    private UnitOfWork uow;

    @Test
    void loadTriggersLazyLoadingForProvidedLazyLoadingProxy() {
        when(uow.isActive()).thenReturn(true);
        final OWLClassC entity = new OWLClassC(Generators.createIndividualIdentifier());
        final FieldSpecification<OWLClassC, List> fieldSpec = mock(FieldSpecification.class);
        final LazyLoadingListProxy proxy = new LazyLoadingListProxy(entity, fieldSpec, uow);
        JOPALazyUtils.load(proxy);
        verify(uow).loadEntityField(entity, fieldSpec);
    }

    @Test
    void loadDoesNothingWhenObjectIsNotLazyLoadingProxy() {
        JOPALazyUtils.load(new OWLClassC(Generators.createIndividualIdentifier()));
        verify(uow, never()).loadEntityField(any(), any());
    }

    @Test
    void loadDoesNothingForNullArgument() {
        assertDoesNotThrow(() -> JOPALazyUtils.load(null));
    }

    @Test
    void isLoadedReturnsFalseForLazyLoadingProxyInstance() {
        final OWLClassC entity = new OWLClassC(Generators.createIndividualIdentifier());
        final FieldSpecification<OWLClassC, List> fieldSpec = mock(FieldSpecification.class);
        final LazyLoadingListProxy proxy = new LazyLoadingListProxy(entity, fieldSpec, uow);
        assertFalse(JOPALazyUtils.isLoaded(proxy));
    }

    @Test
    void isLoadedReturnsTrueWhenObjectIsNotLazyLoadingProxy() {
        assertTrue(JOPALazyUtils.isLoaded(new ArrayList<>()));
    }

    @Test
    void isLoadedReturnsTrueForNullArgument() {
        assertTrue(JOPALazyUtils.isLoaded(null));
    }

    @Test
    void isLoadedReturnsTrueForLazyLoadingProxyInstanceThatIsLoaded() {
        final OWLClassC entity = new OWLClassC(Generators.createIndividualIdentifier());
        final List<OWLClassA> list = List.of(Generators.generateOwlClassAInstance());
        final FieldSpecification<OWLClassC, List> fieldSpec = mock(FieldSpecification.class);
        final LazyLoadingListProxy proxy = new LazyLoadingListProxy(entity, fieldSpec, uow);
        when(uow.isActive()).thenReturn(true);
        when(uow.loadEntityField(entity, fieldSpec)).thenReturn(list);
        proxy.triggerLazyLoading();
        assertTrue(JOPALazyUtils.isLoaded(proxy));
    }

    @Test
    void getClassReturnsEntityClassForLazyLoadingEntityProxy() throws Exception {
        final Class<? extends OWLClassA> proxyCls = new LazyLoadingEntityProxyGenerator().generate(OWLClassA.class);
        final OWLClassA proxy = proxyCls.getDeclaredConstructor().newInstance();
        assertEquals(OWLClassA.class, JOPALazyUtils.getClass(proxy));
    }

    @Test
    void getClassReturnsObjectClassWhenObjectIsNotLazyLoadingEntityProxy() {
        final OWLClassA entity = Generators.generateOwlClassAInstance();
        assertEquals(OWLClassA.class, JOPALazyUtils.getClass(entity));
    }

    @Test
    void getClassThrowsNullPointerExceptionForNullArgument() {
        assertThrows(NullPointerException.class, () -> JOPALazyUtils.getClass(null));
    }
}
