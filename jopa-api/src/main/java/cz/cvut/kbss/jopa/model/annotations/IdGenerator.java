package cz.cvut.kbss.jopa.model.annotations;

import cz.cvut.kbss.jopa.id.IdentifierGenerator;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the identifier generator to use for the entity.
 */
@Documented
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface IdGenerator {

    /**
     * The identifier generator class.
     */
    Class<? extends IdentifierGenerator> value();
}
