package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;

class SesameValueConverter {

    private ValueFactory vf;
    private String language;

    SesameValueConverter(ValueFactory vf, String language) {
        this.vf = vf;
        this.language = language;
    }

    Value toSesameValue(Assertion assertion, cz.cvut.kbss.ontodriver.model.Value<?> val)
            throws SesameDriverException {
        switch (assertion.getType()) {
            case ANNOTATION_PROPERTY:
            case DATA_PROPERTY:
                return SesameUtils.createDataPropertyLiteral(val.getValue(), language, vf);
            case CLASS:
            case OBJECT_PROPERTY:
                return getValueAsSesameUri(val);
            case PROPERTY:
                return resolvePropertyValue(val);
            default:
                // Failsafe
                throw new IllegalArgumentException("Unsupported assertion type " + assertion.getType());
        }
    }

    private org.openrdf.model.URI getValueAsSesameUri(cz.cvut.kbss.ontodriver.model.Value<?> val) throws SesameDriverException {
        try {
            return vf.createURI(val.getValue().toString());
        } catch (IllegalArgumentException e) {
            throw new SesameDriverException(e);
        }
    }

    private org.openrdf.model.Value resolvePropertyValue(cz.cvut.kbss.ontodriver.model.Value<?> val) {
        try {
            return getValueAsSesameUri(val);
        } catch (SesameDriverException e) {
            return SesameUtils.createDataPropertyLiteral(val.getValue(), language, vf);
        }
    }
}