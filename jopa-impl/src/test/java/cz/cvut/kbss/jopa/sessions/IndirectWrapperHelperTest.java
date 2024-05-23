package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingSetProxy;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

@ExtendWith(MockitoExtension.class)
class IndirectWrapperHelperTest {

    @Mock
    private UnitOfWork uow;

    @Test
    void requiresIndirectWrapperReturnsFalseForLazyLoadingCollectionProxies() {
        final OWLClassJ owner = new OWLClassJ(Generators.createIndividualIdentifier());
        assertFalse(IndirectWrapperHelper.requiresIndirectWrapper(new LazyLoadingSetProxy<>(owner, mock(FieldSpecification.class), uow)));
    }
}
