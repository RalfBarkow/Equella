package com.tle.web.sections.standard.renderers;

import java.io.IOException;
import java.util.Map;

import com.tle.web.sections.SectionWriter;
import com.tle.web.sections.js.JSCallable;
import com.tle.web.sections.js.JSExpression;
import com.tle.web.sections.js.generic.expression.ElementByIdExpression;
import com.tle.web.sections.js.generic.expression.PropertyExpression;
import com.tle.web.sections.js.generic.function.DefaultDisableFunction;
import com.tle.web.sections.js.generic.statement.AssignAsFunction;
import com.tle.web.sections.standard.js.JSDisableable;
import com.tle.web.sections.standard.js.JSValueComponent;
import com.tle.web.sections.standard.model.HtmlNumberFieldState;

public class NumberFieldRenderer extends AbstractTextFieldRenderer implements JSDisableable, JSValueComponent
{
	private HtmlNumberFieldState state;

	public NumberFieldRenderer(HtmlNumberFieldState state)
	{
		super(state, "number");
		this.state = state;
	}

	@SuppressWarnings("nls")
	@Override
	protected void prepareFirstAttributes(SectionWriter writer, Map<String, String> attrs) throws IOException
	{
		super.prepareFirstAttributes(writer, attrs);
		attrs.put("value", valueState.getValue());
		// get min max step from state
		if( state.getMax() != null )
		{
			attrs.put("max", String.valueOf(state.getMax()));
		}
		if( state.getMin() != null )
		{
			attrs.put("min", String.valueOf(state.getMin()));
		}
		if( state.getStep() != null )
		{
			attrs.put("step", String.valueOf(state.getStep()));
		}
		else if( state.isAnyStep() )
		{
			attrs.put("step", "any");
		}
	}

	@Override
	public JSCallable createDisableFunction()
	{
		return new DefaultDisableFunction(this);
	}

	@Override
	public JSExpression createGetExpression()
	{
		return PropertyExpression.create(new ElementByIdExpression(this), "value");
	}

	@Override
	public JSCallable createSetFunction()
	{
		return new AssignAsFunction(createGetExpression());
	}

	@Override
	public JSCallable createResetFunction()
	{
		return new AssignAsFunction(createGetExpression(), PropertyExpression.create(new ElementByIdExpression(this),
			"defaultValue")); //$NON-NLS-1$
	}
}