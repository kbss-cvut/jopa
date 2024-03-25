package cz.cvut.kbss.jopa.proxy.lazy;

import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exception.LazyLoadingException;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class LazyLoadingMapProxyTest {

    @Mock
    private UnitOfWork uow;

    @Mock
    private PropertiesSpecification<OWLClassB, Map<String, Set<String>>, String, String> fieldSpec;

    private Map<String, Set<String>> proxiedProperties;

    private OWLClassB entity;

    private LazyLoadingMapProxy<OWLClassB, String, Set<String>> sut;

    @BeforeEach
    void setUp() {
        this.proxiedProperties = spy(Generators.generateStringProperties());
        this.entity = new OWLClassB(Generators.createIndividualIdentifier());
        this.sut = new LazyLoadingMapProxy<>(entity, fieldSpec, uow);
    }

    @Test
    void triggerLazyLoadingThrowsLazyLoadingExceptionWhenNoPersistenceContextIsAvailable() {
        this.sut = new LazyLoadingMapProxy<>(entity, fieldSpec, null);
        assertThrows(LazyLoadingException.class, () -> sut.triggerLazyLoading());
    }

    @Test
    void triggerLazyLoadingThrowsLazyLoadingExceptionWhenPersistenceContextIsNotActiveAnymore() {
        when(uow.isActive()).thenReturn(false);
        assertThrows(LazyLoadingException.class, () -> sut.triggerLazyLoading());
    }

    @Test
    void sizeTriggersLazyLoadingAndReturnsLoadedMapSizeResult() {
        initLazyLoading();
        assertEquals(proxiedProperties.size(), sut.size());
        verify(uow).loadEntityField(entity, fieldSpec);
    }

    private void initLazyLoading() {
        entity.setProperties(sut);
        when(uow.isActive()).thenReturn(true);
        when(uow.loadEntityField(entity, fieldSpec)).thenAnswer(inv -> {
            final OWLClassB owner = inv.getArgument(0);
            owner.setProperties(proxiedProperties);
            return proxiedProperties;
        });
    }

    @Test
    void isEmptyTriggersLazyLoadingAndReturnsMapIsEmptyResult() {
        initLazyLoading();
        assertEquals(proxiedProperties.isEmpty(), sut.isEmpty());
        verify(uow).loadEntityField(entity, fieldSpec);
    }

    @Test
    void containsKeyTriggersLazyLoadingAndReturnsMapContainsKeyResult() {
        initLazyLoading();
        final String key = Generators.createIndividualIdentifier().toString();
        assertEquals(proxiedProperties.containsKey(key), sut.containsKey(key));
        verify(uow).loadEntityField(entity, fieldSpec);
    }

    @Test
    void containsValueTriggersLazyLoadingAndReturnsMapContainsValueResult() {
        initLazyLoading();
        final Set<String> value = proxiedProperties.values().iterator().next();
        assertEquals(proxiedProperties.containsValue(value), sut.containsValue(value));
        verify(uow).loadEntityField(entity, fieldSpec);
    }

    @Test
    void getTriggersLazyLoadingAndReturnsMapGetResult() {
        initLazyLoading();
        final String key = proxiedProperties.keySet().iterator().next();
        assertEquals(proxiedProperties.get(key), sut.get(key));
        verify(uow).loadEntityField(entity, fieldSpec);
    }

    @Test
    void putTriggersLazyLoadingAndReturnsMapPutResult() {
        initLazyLoading();
        final String key = Generators.createPropertyIdentifier().toString();
        final Set<String> values = Set.of("1", "2", Generators.createIndividualIdentifier().toString());
        sut.put(key, values);
        assertEquals(values, proxiedProperties.get(key));
        verify(uow).loadEntityField(entity, fieldSpec);
        verify(proxiedProperties).put(key, values);
    }

    @Test
    void removeTriggersLazyLoadingAndReturnsMapRemoveResult() {
        initLazyLoading();
        final String key = proxiedProperties.keySet().iterator().next();
        assertEquals(proxiedProperties.get(key), sut.remove(key));
        verify(uow).loadEntityField(entity, fieldSpec);
        verify(proxiedProperties).remove(key);
    }

    @Test
    void putAllTriggersLazyLoadingAndPutsAllArgumentsToLoadedMap() {
        initLazyLoading();
        final Map<String, Set<String>> toPut = Generators.generateStringProperties(2, 2);
        sut.putAll(toPut);
        verify(uow).loadEntityField(entity, fieldSpec);
        verify(proxiedProperties).putAll(toPut);
    }

    @Test
    void clearTriggersLazyLoadingAndClearsLoadedMap() {
        initLazyLoading();
        sut.clear();
        verify(uow).loadEntityField(entity, fieldSpec);
        verify(proxiedProperties).clear();
    }

    @Test
    void keySetTriggersLazyLoadingAndReturnsMapKeySetResult() {
        initLazyLoading();
        assertEquals(proxiedProperties.keySet(), sut.keySet());
        verify(uow).loadEntityField(entity, fieldSpec);
    }

    @Test
    void valuesTriggersLazyLoadingAndReturnsMapValues() {
        initLazyLoading();
        assertEquals(proxiedProperties.values(), sut.values());
        verify(uow).loadEntityField(entity, fieldSpec);
    }

    @Test
    void entrySetTriggersLazyLoadingAndReturnsMapValues() {
        initLazyLoading();
        assertEquals(proxiedProperties.entrySet(), sut.entrySet());
        verify(uow).loadEntityField(entity, fieldSpec);
    }
}
