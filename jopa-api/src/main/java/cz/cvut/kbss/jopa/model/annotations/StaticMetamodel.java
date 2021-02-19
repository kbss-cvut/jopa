package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies that the annotated class is a metamodel class that represents the entity or mapped superclass designated by the value element.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface StaticMetamodel {

    /**
     * Class being modeled by the annotated class.
     */
    Class<?> value();
}
