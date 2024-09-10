package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.RDFContainerType;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class RdfContainerAttributeImplTest {

    @Test
    void getCollectionTypeReturnsListForRdfSeq() {
        final DataPropertyAttributes pa = new DataPropertyAttributes(null);
        pa.type = BasicTypeImpl.get(Integer.class);
        pa.iri = IRI.create(Vocabulary.p_p_gender);
        pa.cascadeTypes = new CascadeType[0];
        pa.persistentAttributeType = Attribute.PersistentAttributeType.DATA;

        final RdfContainerAttributeImpl<?, ?, ?> sut = (RdfContainerAttributeImpl<?, ?, ?>) RdfContainerAttributeImpl.builder(pa)
                                                                                                                     .containerType(RDFContainerType.SEQ)
                                                                                                                     .elementType(BasicTypeImpl.get(Integer.class))
                                                                                                                     .collectionType(List.class)
                                                                                                                     .build();
        assertEquals(CollectionType.LIST, sut.getCollectionType());
    }

    @Test
    void getCollectionTypeReturnsSetForRdfAlt() {
        final DataPropertyAttributes pa = new DataPropertyAttributes(null);
        pa.type = BasicTypeImpl.get(Integer.class);
        pa.iri = IRI.create(Vocabulary.p_p_gender);
        pa.cascadeTypes = new CascadeType[0];
        pa.persistentAttributeType = Attribute.PersistentAttributeType.DATA;

        final RdfContainerAttributeImpl<?, ?, ?> sut = (RdfContainerAttributeImpl<?, ?, ?>) RdfContainerAttributeImpl.builder(pa)
                                                                                                                     .containerType(RDFContainerType.ALT)
                                                                                                                     .elementType(BasicTypeImpl.get(Integer.class))
                                                                                                                     .collectionType(List.class)
                                                                                                                     .build();
        assertEquals(CollectionType.SET, sut.getCollectionType());
    }

    @Test
    void getCollectionTypeReturnsCollectionForRdfBag() {
        final DataPropertyAttributes pa = new DataPropertyAttributes(null);
        pa.type = BasicTypeImpl.get(Integer.class);
        pa.iri = IRI.create(Vocabulary.p_p_gender);
        pa.cascadeTypes = new CascadeType[0];
        pa.persistentAttributeType = Attribute.PersistentAttributeType.DATA;

        final RdfContainerAttributeImpl<?, ?, ?> sut = (RdfContainerAttributeImpl<?, ?, ?>) RdfContainerAttributeImpl.builder(pa)
                                                                                                                     .containerType(RDFContainerType.BAG)
                                                                                                                     .elementType(BasicTypeImpl.get(Integer.class))
                                                                                                                     .collectionType(List.class)
                                                                                                                     .build();
        assertEquals(CollectionType.COLLECTION, sut.getCollectionType());
    }
}
