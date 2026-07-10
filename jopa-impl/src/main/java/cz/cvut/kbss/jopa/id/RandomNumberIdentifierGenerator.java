package cz.cvut.kbss.jopa.id;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;

import java.net.URI;
import java.util.Objects;
import java.util.Random;

public class RandomNumberIdentifierGenerator extends AbstractIdentifierGenerator {

    private static final int GENERATION_THRESHOLD = 64;

    private static final Random RANDOM = new Random();

    @Override
    public <T> URI generate(Object entity, EntityType<T> entityClass, Connection connection) {
        Objects.requireNonNull(entity);
        Objects.requireNonNull(entityClass);
        Objects.requireNonNull(connection);

        final URI classUri = entityClass.getIRI().toURI();
        int counter = 0;
        while (counter++ < GENERATION_THRESHOLD) {
            final URI id = generate(classUri);
            if (!exists(id, classUri, connection)) {
                return id;
            }
        }
        throw new IdentifierGenerationException("Unable to generate a unique identifier.");
    }

    private URI generate(URI classUri) {
        if (classUri.getFragment() != null) {
            return URI.create(classUri + "_instance" + RANDOM.nextInt());
        } else {
            String base = classUri.toString();
            if (base.endsWith("/")) {
                return URI.create(base + "instance" + RANDOM.nextInt());
            } else {
                return URI.create(base + "/instance" + RANDOM.nextInt());
            }
        }
    }
}
