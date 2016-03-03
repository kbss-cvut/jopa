package cz.cvut.kbss.ontodriver.owlapi.util;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.net.URI;
import java.util.Map;

public class DefaultOntologyIriMapper implements OWLOntologyIRIMapper {

    private final Map<URI, URI> mappings;

    public DefaultOntologyIriMapper(MappingFileParser parser) {
        this.mappings = parser.getMappings();
    }

    @Nullable
    @Override
    public IRI getDocumentIRI(@Nonnull IRI ontologyIRI) {
        return null;
    }
}
