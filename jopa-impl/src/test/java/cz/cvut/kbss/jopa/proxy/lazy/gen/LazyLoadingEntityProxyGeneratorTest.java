package cz.cvut.kbss.jopa.proxy.lazy.gen;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.Mockito.doAnswer;
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
}
