package cz.cvut.kbss.jopa.proxy.reference;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.EntityNotFoundException;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class EntityReferenceProxyGeneratorTest {

    @Mock
    private UnitOfWork uow;

    private final OWLClassA storedInstance = Generators.generateOwlClassAInstance();

    private final EntityReferenceProxyGenerator sut = new EntityReferenceProxyGenerator();

    @Test
    void generateGeneratesProxyClassImplementingCorrectInterfaces() {
        final Class<? extends OWLClassA> result = sut.generate(OWLClassA.class);
        assertTrue(EntityReferenceProxy.class.isAssignableFrom(result));
    }

    @Test
    void generateGeneratesProxyClassThatTriggersLoadingWhenGetterIsAccessedAndReturnsLoadedValue() throws Exception {
        final Class<? extends OWLClassA> result = sut.generate(OWLClassA.class);
        final OWLClassA instance = result.getConstructor().newInstance();
        initReferenceLoading(instance);
        initMetamodel();
        assertEquals(storedInstance.getStringAttribute(), instance.getStringAttribute());
        verify(uow).readObject(OWLClassA.class, storedInstance.getUri(), null);
    }

    private void initReferenceLoading(OWLClassA instance) {
        assertInstanceOf(EntityReferenceProxy.class, instance);
        final EntityReferenceProxy<OWLClassA> aInstance = (EntityReferenceProxy<OWLClassA>) instance;
        aInstance.setPersistenceContext(uow);
        aInstance.setIdentifier(storedInstance.getUri());
        aInstance.setType(OWLClassA.class);
        when(uow.readObject(OWLClassA.class, storedInstance.getUri(), null)).thenReturn(storedInstance);
        when(uow.isActive()).thenReturn(true);
    }

    private void initMetamodel() throws Exception {
        final MetamodelImpl mm = mock(MetamodelImpl.class);
        when(uow.getMetamodel()).thenReturn(mm);
        final IdentifiableEntityType<OWLClassA> et = mock(IdentifiableEntityType.class);
        when(mm.entity(OWLClassA.class)).thenReturn(et);
        final Identifier id = mock(Identifier.class);
        when(id.getName()).thenReturn(OWLClassA.class.getDeclaredField("uri").getName());
        when(et.getIdentifier()).thenReturn(id);
    }

    @Test
    void generateGeneratesProxyClassThatReturnsLoadedValueOnSubsequentGetterCalls() throws Exception {
        final Class<? extends OWLClassA> result = sut.generate(OWLClassA.class);
        final OWLClassA instance = result.getConstructor().newInstance();
        initReferenceLoading(instance);
        initMetamodel();
        assertEquals(storedInstance.getStringAttribute(), instance.getStringAttribute());
        assertTrue(((EntityReferenceProxy<OWLClassA>) instance).isLoaded());
        assertEquals(storedInstance.getStringAttribute(), instance.getStringAttribute());
        assertEquals(storedInstance.getStringAttribute(), instance.getStringAttribute());
        verify(uow).readObject(OWLClassA.class, storedInstance.getUri(), null);
    }

    @Test
    void generateGeneratesProxyClassThatTriggersLoadingWhenSetterIsAccessedAndPassesValueToLoadedInstance() throws Exception {
        final Class<? extends OWLClassA> result = sut.generate(OWLClassA.class);
        final OWLClassA instance = result.getConstructor().newInstance();
        initReferenceLoading(instance);
        initMetamodel();
        final String newString = "random string " + Generators.randomInt();
        instance.setStringAttribute(newString);
        verify(uow).readObject(OWLClassA.class, storedInstance.getUri(), null);
        assertEquals(newString, storedInstance.getStringAttribute());
    }

    @Test
    void generateGeneratesProxyClassThatUsesLoadedInstanceOnSubsequentSetterCalls() throws Exception {
        final Class<? extends OWLClassA> result = sut.generate(OWLClassA.class);
        final OWLClassA instance = result.getConstructor().newInstance();
        initReferenceLoading(instance);
        initMetamodel();
        final List<String> values = List.of(Integer.toString(Generators.randomInt()), Integer.toString(Generators.randomInt()), Integer.toString(Generators.randomInt()));
        values.forEach(instance::setStringAttribute);
        verify(uow).readObject(OWLClassA.class, storedInstance.getUri(), null);
        assertEquals(values.get(values.size() - 1), instance.getStringAttribute());
        assertEquals(values.get(values.size() - 1), storedInstance.getStringAttribute());
    }

    @Test
    void generateGeneratesProxyClassThatThrowsEntityNotFoundExceptionWhenLoadingOnGetterCallReturnsNull() throws Exception {
        initMetamodel();
        final Class<? extends OWLClassA> result = sut.generate(OWLClassA.class);
        final OWLClassA instance = result.getConstructor().newInstance();
        final EntityReferenceProxy<OWLClassA> aInstance = (EntityReferenceProxy<OWLClassA>) instance;
        aInstance.setPersistenceContext(uow);
        aInstance.setIdentifier(storedInstance.getUri());
        aInstance.setType(OWLClassA.class);
        when(uow.isActive()).thenReturn(true);
        assertThrows(EntityNotFoundException.class, instance::getStringAttribute);
    }

    @Test
    void generateGeneratesProxyClassThatThrowsEntityNotFoundExceptionWhenLoadingOnSetterCallReturnsNull() throws Exception {
        initMetamodel();
        final Class<? extends OWLClassA> result = sut.generate(OWLClassA.class);
        final OWLClassA instance = result.getConstructor().newInstance();
        final EntityReferenceProxy<OWLClassA> aInstance = (EntityReferenceProxy<OWLClassA>) instance;
        aInstance.setPersistenceContext(uow);
        aInstance.setIdentifier(storedInstance.getUri());
        aInstance.setType(OWLClassA.class);
        when(uow.isActive()).thenReturn(true);
        assertThrows(EntityNotFoundException.class, () -> instance.setTypes(Set.of(Generators.createIndividualIdentifier()
                                                                                             .toString())));
    }

    @Test
    void generateGeneratesProxyClassThatDoesNotTriggerLoadingOnIdentifierGetter() throws Exception {
        initMetamodel();
        final Class<? extends OWLClassA> result = sut.generate(OWLClassA.class);
        final OWLClassA instance = result.getConstructor().newInstance();
        final EntityReferenceProxy<OWLClassA> aInstance = (EntityReferenceProxy<OWLClassA>) instance;
        aInstance.setPersistenceContext(uow);
        aInstance.setIdentifier(storedInstance.getUri());
        aInstance.setType(OWLClassA.class);
        assertEquals(storedInstance.getUri(), instance.getUri());
        verify(uow, never()).readObject(OWLClassA.class, storedInstance.getUri(), null);
    }

    // This makes no sense, but let's be sure we are handling it
    @Test
    void generateGeneratesProxyClassThatDoesNotTriggerLoadingOnIdentifierSetter() throws Exception {
        initMetamodel();
        final Class<? extends OWLClassA> result = sut.generate(OWLClassA.class);
        final OWLClassA instance = result.getConstructor().newInstance();
        final EntityReferenceProxy<OWLClassA> aInstance = (EntityReferenceProxy<OWLClassA>) instance;
        aInstance.setPersistenceContext(uow);
        aInstance.setIdentifier(storedInstance.getUri());
        aInstance.setType(OWLClassA.class);
        final URI newId = Generators.createIndividualIdentifier();
        instance.setUri(newId);
        assertEquals(newId, aInstance.getIdentifier());
        verify(uow, never()).readObject(OWLClassA.class, storedInstance.getUri(), null);
    }
}
