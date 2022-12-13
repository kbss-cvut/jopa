package cz.cvut.kbss.jopa.model.annotations.util;

import cz.cvut.kbss.jopa.NonJPA;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates that the annotated type should not be considered a JOPA entity.
 * <p>
 * This allows to exclude classes annotated with {@link cz.cvut.kbss.jopa.model.annotations.OWLClass} from persistence.
 */
@NonJPA
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface NotEntity {
}
