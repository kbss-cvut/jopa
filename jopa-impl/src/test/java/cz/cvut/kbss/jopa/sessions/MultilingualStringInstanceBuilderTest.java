package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectMultilingualString;
import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;

class MultilingualStringInstanceBuilderTest {

    private final Descriptor descriptor = new EntityDescriptor();

    private MultilingualStringInstanceBuilder sut;

    @BeforeEach
    void setUp() {
        this.sut = new MultilingualStringInstanceBuilder(mock(CloneBuilderImpl.class), mock(UnitOfWorkImpl.class));
    }

    @Test
    void buildCloneCopiesAllTranslationsInSpecifiedOriginalMultilingualString() throws Exception {
        final MultilingualString original = MultilingualString.create("building", Generators.LANG);
        original.set("cs", "stavba");
        final Object result = sut.buildClone(new OWLClassU(), OWLClassU.getSingularStringAttField(), original,
                new CloneConfiguration(descriptor));
        assertThat(result, instanceOf(MultilingualString.class));
        final MultilingualString typedResult = (MultilingualString) result;
        assertEquals(original.getValue(), typedResult.getValue());
    }

    @Test
    void buildCloneResultsNullWhenOriginalIsNull() throws Exception {
        final Object result = sut.buildClone(new OWLClassU(), OWLClassU.getSingularStringAttField(), null,
                new CloneConfiguration(descriptor));
        assertNull(result);
    }

    @Test
    void buildCloneReturnsIndirectWrapperAllowingToTrackModifyingOperations() throws Exception {
        final MultilingualString original = MultilingualString.create("building", Generators.LANG);
        original.set("cs", "stavba");
        final Object result = sut.buildClone(new OWLClassU(), OWLClassU.getSingularStringAttField(), original,
                new CloneConfiguration(descriptor));
        assertThat(result, instanceOf(IndirectMultilingualString.class));
    }

    @Test
    void buildCloneBuildsCloneOfWrappedMultilingualStringWhenArgumentIsIndirectMultilingualString() throws Exception {
        final MultilingualString original = MultilingualString.create("building", Generators.LANG);
        final IndirectMultilingualString arg = new IndirectMultilingualString(new OWLClassU(),
                OWLClassU.getSingularStringAttField(), mock(UnitOfWorkImpl.class), original);
        final Object result = sut.buildClone(new OWLClassU(), OWLClassU.getSingularStringAttField(), arg, new CloneConfiguration(descriptor));
        assertThat(result, instanceOf(IndirectMultilingualString.class));
        final IndirectMultilingualString resultIndirect = (IndirectMultilingualString) result;
        assertEquals(original, resultIndirect.unwrap());
    }

    @Test
    void mergeChangesCopiesValueOfCloneToOriginal() throws Exception {
        final MultilingualString clone = MultilingualString.create("building", Generators.LANG);
        clone.set("cs", "stavba");
        final MultilingualString original = MultilingualString.create("construction", Generators.LANG);
        final OWLClassU target = new OWLClassU();
        target.setSingularStringAtt(original);
        sut.mergeChanges(OWLClassU.getSingularStringAttField(), target, original, clone);
        assertEquals(clone.getValue(), target.getSingularStringAtt().getValue());
    }

    @Test
    void mergeChangesSupportsCloneBeingNull() throws Exception {
        final MultilingualString original = MultilingualString.create("construction", Generators.LANG);
        final OWLClassU target = new OWLClassU();
        target.setSingularStringAtt(original);
        sut.mergeChanges(OWLClassU.getSingularStringAttField(), target, original, null);
        assertNull(target.getSingularStringAtt());
    }
}
