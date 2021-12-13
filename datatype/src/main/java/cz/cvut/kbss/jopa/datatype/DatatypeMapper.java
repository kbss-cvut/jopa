package cz.cvut.kbss.jopa.datatype;


import cz.cvut.kbss.ontodriver.model.Literal;

import java.util.Optional;

/**
 * Maps RDF literals to Java objects.
 */
public interface DatatypeMapper {

    /**
     * Maps the specified RDF literal to a suitable basic (in terms of JOPA OOM) Java type.
     *
     * @param literal Literal to map
     * @return Mapped value wrapped in an {@code Optional}, empty Optional if no suitable mapping can be found
     */
    Optional<Object> map(Literal literal);
}
