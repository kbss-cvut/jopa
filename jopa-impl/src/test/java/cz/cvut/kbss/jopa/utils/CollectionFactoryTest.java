package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.List;
import java.util.Set;

import static org.junit.Assert.*;

public class CollectionFactoryTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void createDefaultCollectionReturnsListImplementationForListType() {
        assertTrue(CollectionFactory.createDefaultCollection(PluralAttribute.CollectionType.LIST) instanceof List);
    }

    @Test
    public void createDefaultCollectionReturnsSetImplementationForSetType() {
        assertTrue(CollectionFactory.createDefaultCollection(PluralAttribute.CollectionType.SET) instanceof Set);
    }

    @Test
    public void createDefaultCollectionReturnsSetImplementationForCollectionType() {
        assertTrue(CollectionFactory.createDefaultCollection(PluralAttribute.CollectionType.COLLECTION) instanceof Set);
    }

    @Test
    public void createDefaultCollectionThrowsIllegalArgumentForMapType() {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Collection type " + PluralAttribute.CollectionType.MAP + " is not supported.");
        CollectionFactory.createDefaultCollection(PluralAttribute.CollectionType.MAP);
    }
}