package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;

class IriParameterValue extends AbstractParameterValue {

    private final IRI iri;

    public IriParameterValue(IRI iri) {this.iri = iri;}

    @Override
    public IRI getValue() {
        return iri;
    }

    @Override
    public String getQueryString() {
        return IdentifierTransformer.stringifyIri(iri);
    }
}
