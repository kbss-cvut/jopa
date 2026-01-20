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
package cz.cvut.kbss.jopa.proxy.lazy.gen;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassG;
import cz.cvut.kbss.jopa.environment.OWLClassH;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Method;
import java.util.stream.IntStream;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class LazyLoadingEntityProxyGeneratorTest {

    @Mock
    private UnitOfWork uow;

    @Mock
    private FieldSpecification<OWLClassD, OWLClassA> fieldSpec;

    private final OWLClassD owner = new OWLClassD(Generators.createIndividualIdentifier());

    private final OWLClassA loaded = Generators.generateOwlClassAInstance();

    private final LazyLoadingEntityProxyGenerator sut = new LazyLoadingEntityProxyGenerator();

    @Test
    void generateGeneratesProxyThatTriggersLazyLoadingForGetterAndReturnsLoadedInstance() throws Exception {
        initLazyLoading();
        final Class<? extends OWLClassA> resultCls = sut.generate(OWLClassA.class);
        final OWLClassA proxy = resultCls.getDeclaredConstructor().newInstance();
        ((LazyLoadingProxyPropertyAccessor) proxy).setPersistenceContext(uow);
        ((LazyLoadingProxyPropertyAccessor) proxy).setFieldSpec(fieldSpec);
        ((LazyLoadingProxyPropertyAccessor) proxy).setOwner(owner);
        assertInstanceOf(LazyLoadingEntityProxy.class, proxy);
        owner.setOwlClassA(proxy);
        assertEquals(loaded.getUri(), owner.getOwlClassA().getUri());
        assertEquals(loaded, owner.getOwlClassA());
        verify(uow).loadEntityField(owner, fieldSpec);
    }

    private void initLazyLoading() {
        doAnswer(inv -> {
            final OWLClassD owner = inv.getArgument(0);
            owner.setOwlClassA(loaded);
            return loaded;
        }).when(uow).loadEntityField(owner, fieldSpec);
        when(uow.isActive()).thenReturn(true);
    }

    @Test
    void generateGeneratesProxyThatTriggersLazyLoadingForSetterAndSetsProvidedArgumentOnLoadedInstance() throws Exception {
        initLazyLoading();
        final Class<? extends OWLClassA> resultCls = sut.generate(OWLClassA.class);
        final OWLClassA proxy = resultCls.getDeclaredConstructor().newInstance();
        ((LazyLoadingProxyPropertyAccessor) proxy).setPersistenceContext(uow);
        ((LazyLoadingProxyPropertyAccessor) proxy).setFieldSpec(fieldSpec);
        ((LazyLoadingProxyPropertyAccessor) proxy).setOwner(owner);
        assertInstanceOf(LazyLoadingEntityProxy.class, proxy);
        owner.setOwlClassA(proxy);
        final String setValue = "Value set on lazily loaded reference";
        owner.getOwlClassA().setStringAttribute(setValue);
        verify(uow).loadEntityField(owner, fieldSpec);
        assertEquals(setValue, loaded.getStringAttribute());
    }

    @Test
    void generateGeneratesProxyWithCommonToStringMethod() throws Exception {
        when(fieldSpec.getName()).thenReturn("owlClassA");
        final Class<? extends OWLClassA> resultCls = sut.generate(OWLClassA.class);
        final OWLClassA proxy = resultCls.getDeclaredConstructor().newInstance();
        ((LazyLoadingProxyPropertyAccessor) proxy).setPersistenceContext(uow);
        ((LazyLoadingProxyPropertyAccessor) proxy).setFieldSpec(fieldSpec);
        ((LazyLoadingProxyPropertyAccessor) proxy).setOwner(owner);
        assertInstanceOf(LazyLoadingEntityProxy.class, proxy);
        final String result = proxy.toString();
        assertThat(result, containsString(OWLClassD.class.getSimpleName() + ".owlClassA"));
        assertThat(result, containsString(resultCls.getSimpleName()));
        assertEquals(((LazyLoadingEntityProxy<?>) proxy).stringify(), result);
    }

    @Test
    void getLoadedValueOnGeneratedProxyReturnsLoadedValueAfterTriggeringLazyLoading() throws Exception {
        initLazyLoading();
        final Class<? extends OWLClassA> resultCls = sut.generate(OWLClassA.class);
        final OWLClassA proxy = resultCls.getDeclaredConstructor().newInstance();
        ((LazyLoadingProxyPropertyAccessor) proxy).setPersistenceContext(uow);
        ((LazyLoadingProxyPropertyAccessor) proxy).setFieldSpec(fieldSpec);
        ((LazyLoadingProxyPropertyAccessor) proxy).setOwner(owner);
        assertEquals(loaded.getStringAttribute(), proxy.getStringAttribute());
        assertTrue(((LazyLoadingEntityProxy<OWLClassA>) proxy).isLoaded());
        assertEquals(loaded, ((LazyLoadingEntityProxy<OWLClassA>) proxy).getLoadedValue());
    }

    @Test
    void triggerLazyLoadingMultipleTimesOnGeneratedProxyReturnsAlreadyLoadedValueAndDoesNotCallPersistenceContext() throws Exception {
        initLazyLoading();
        final Class<? extends OWLClassA> resultCls = sut.generate(OWLClassA.class);
        final OWLClassA proxy = resultCls.getDeclaredConstructor().newInstance();
        ((LazyLoadingProxyPropertyAccessor) proxy).setPersistenceContext(uow);
        ((LazyLoadingProxyPropertyAccessor) proxy).setFieldSpec(fieldSpec);
        ((LazyLoadingProxyPropertyAccessor) proxy).setOwner(owner);
        IntStream.range(0, 5)
                 .forEach(i -> assertEquals(loaded, ((LazyLoadingEntityProxy<OWLClassA>) proxy).triggerLazyLoading()));
        verify(uow, times(1)).loadEntityField(owner, fieldSpec);
    }

    @Test
    void getLoadedOnGeneratedProxyThrowsIllegalStateExceptionWhenCalledBeforeLoading() throws Exception {
        final Class<? extends OWLClassA> resultCls = sut.generate(OWLClassA.class);
        final OWLClassA proxy = resultCls.getDeclaredConstructor().newInstance();
        ((LazyLoadingProxyPropertyAccessor) proxy).setPersistenceContext(uow);
        ((LazyLoadingProxyPropertyAccessor) proxy).setFieldSpec(fieldSpec);
        ((LazyLoadingProxyPropertyAccessor) proxy).setOwner(owner);
        assertThrows(IllegalStateException.class, () -> ((LazyLoadingEntityProxy<OWLClassA>) proxy).getLoadedValue());
    }

    @Test
    void generateGeneratesProxyWithEqualsTriggeringLazyLoadingWhenEntityClassOverridesEquals() throws Exception {
        final Class<? extends OWLClassH> resultCls = sut.generate(OWLClassH.class);
        final OWLClassH hLoaded = new OWLClassH(Generators.createIndividualIdentifier());
        final OWLClassG gOwner = new OWLClassG(Generators.createIndividualIdentifier());
        final FieldSpecification<OWLClassG, OWLClassH> fs = mock(FieldSpecification.class);
        doAnswer(inv -> {
            final OWLClassG owner = inv.getArgument(0);
            owner.setOwlClassH(hLoaded);
            return hLoaded;
        }).when(uow).loadEntityField(gOwner, fs);
        when(uow.isActive()).thenReturn(true);

        final OWLClassH proxy = resultCls.getDeclaredConstructor().newInstance();
        assertEquals(resultCls, resultCls.getDeclaredMethod("equals", Object.class).getDeclaringClass());
        ((LazyLoadingProxyPropertyAccessor) proxy).setPersistenceContext(uow);
        ((LazyLoadingProxyPropertyAccessor) proxy).setFieldSpec(fs);
        ((LazyLoadingProxyPropertyAccessor) proxy).setOwner(gOwner);
        assertFalse(proxy.equals(new OWLClassH(Generators.createIndividualIdentifier())));
        verify(uow).loadEntityField(gOwner, fs);
    }

    @Test
    void generateGeneratesProxyWithHashCodeTriggeringLazyLoadingWhenEntityClassOverridesHashCode() throws Exception {
        final Class<? extends OWLClassH> resultCls = sut.generate(OWLClassH.class);
        final OWLClassH hLoaded = new OWLClassH(Generators.createIndividualIdentifier());
        final OWLClassG gOwner = new OWLClassG(Generators.createIndividualIdentifier());
        final FieldSpecification<OWLClassG, OWLClassH> fs = mock(FieldSpecification.class);
        doAnswer(inv -> {
            final OWLClassG owner = inv.getArgument(0);
            owner.setOwlClassH(hLoaded);
            return hLoaded;
        }).when(uow).loadEntityField(gOwner, fs);
        when(uow.isActive()).thenReturn(true);

        final OWLClassH proxy = resultCls.getDeclaredConstructor().newInstance();
        assertEquals(resultCls, resultCls.getDeclaredMethod("hashCode").getDeclaringClass());
        ((LazyLoadingProxyPropertyAccessor) proxy).setPersistenceContext(uow);
        ((LazyLoadingProxyPropertyAccessor) proxy).setFieldSpec(fs);
        ((LazyLoadingProxyPropertyAccessor) proxy).setOwner(gOwner);
        assertEquals(hLoaded.hashCode(), proxy.hashCode());
        verify(uow).loadEntityField(gOwner, fs);
    }

    @Test
    void generateGeneratesProxyWithoutOverriddenEqualsAndHashCodeWhenEntityClassDoesNotOverrideThem() throws Exception {
        final Class<? extends OWLClassA> resultCls = sut.generate(OWLClassA.class);
        final Method equals = resultCls.getMethod("equals", Object.class);
        assertEquals(Object.class, equals.getDeclaringClass());
        final Method hashCode = resultCls.getMethod("hashCode");
        assertEquals(Object.class, hashCode.getDeclaringClass());
    }
}
