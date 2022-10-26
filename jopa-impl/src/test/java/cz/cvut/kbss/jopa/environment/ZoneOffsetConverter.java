package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.AttributeConverter;
import cz.cvut.kbss.jopa.model.annotations.Converter;

import java.time.ZoneOffset;

@Converter
public class ZoneOffsetConverter implements AttributeConverter<ZoneOffset, String> {

    @Override
    public String convertToAxiomValue(ZoneOffset value) {
        return value.getId();
    }

    @Override
    public ZoneOffset convertToAttribute(String value) {
        return ZoneOffset.of(value);
    }
}
