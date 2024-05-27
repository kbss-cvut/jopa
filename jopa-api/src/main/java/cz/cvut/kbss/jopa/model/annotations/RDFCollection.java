package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies mapping of an <a href="https://www.w3.org/TR/rdf12-schema/#ch_collectionvocab">RDF collection</a>.
 * <p>
 * RDF collections are lists of items where each item points to a value and the next item. In this regard, they are
 * equivalent to a {@link Sequence} of type {@link SequenceType#referenced}. In contrast to referenced lists though, RDF
 * collections are closed by a {@literal rdf:nil} element.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.METHOD})
public @interface RDFCollection {
}
