package com.tle.core.reporting;

import java.math.BigDecimal;
import java.util.List;

import org.eclipse.datatools.connectivity.oda.OdaException;

import com.tle.reporting.MetadataBean;

public class SimpleResultSet extends AbstractResultSet
{
	private Object[][] data;
	private int maxRows;
	private int currentRow = -1;
	private boolean wasnull;

	public SimpleResultSet(List<Object[]> data, MetadataBean metadata)
	{
		this(data.toArray(new Object[data.size()][]), metadata);
	}

	public SimpleResultSet(Object[][] data, MetadataBean metadata)
	{
		super(metadata);
		if( data == null )
		{
			data = new Object[0][];
		}
		this.data = data;
		maxRows = data.length;
	}

	@Override
	public void close() throws OdaException
	{
		// Don't care
	}

	@Override
	public BigDecimal getBigDecimal(int i) throws OdaException
	{
		return (BigDecimal) getCol(i);
	}

	@Override
	protected Object getCol(int i)
	{
		Object obj = data[currentRow][i - 1];
		wasnull = obj == null;
		return obj;
	}

	@Override
	public int getRow() throws OdaException
	{
		return currentRow + 1;
	}

	@Override
	public boolean next() throws OdaException
	{
		return !(++currentRow >= maxRows);
	}

	@Override
	public void setMaxRows(int max) throws OdaException
	{
		maxRows = Math.min(data.length, max);
	}

	@Override
	public boolean wasNull() throws OdaException
	{
		return wasnull;
	}

	@Override
	public Object getObject(String colName) throws OdaException
	{
		return getObject(findColumn(colName));
	}

}