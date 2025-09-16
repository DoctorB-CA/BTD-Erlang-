#!/usr/bin/env python3
"""
Markdown to HTML converter for BTD-Erlang project documentation
"""

import markdown
import sys
import os

def convert_md_to_html(md_file_path, output_path=None):
    """Convert markdown file to HTML"""
    
    if not os.path.exists(md_file_path):
        print(f"Error: File {md_file_path} not found!")
        return False
    
    # Read the markdown file
    with open(md_file_path, 'r', encoding='utf-8') as f:
        md_content = f.read()
    
    # Configure markdown with extensions for better formatting
    md = markdown.Markdown(extensions=[
        'markdown.extensions.toc',
        'markdown.extensions.tables',
        'markdown.extensions.codehilite',
        'markdown.extensions.fenced_code'
    ])
    
    # Convert to HTML
    html_body = md.convert(md_content)
    
    # Create complete HTML document with CSS styling
    html_template = """<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>BTD-Erlang Project Documentation</title>
    <style>
        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            line-height: 1.6;
            max-width: 1000px;
            margin: 0 auto;
            padding: 20px;
            color: #333;
        }}
        
        h1 {{
            color: #2c3e50;
            border-bottom: 3px solid #3498db;
            padding-bottom: 10px;
        }}
        
        h2 {{
            color: #34495e;
            border-bottom: 2px solid #ecf0f1;
            padding-bottom: 5px;
            margin-top: 30px;
        }}
        
        h3 {{
            color: #34495e;
            margin-top: 25px;
        }}
        
        h4 {{
            color: #7f8c8d;
            margin-top: 20px;
        }}
        
        code {{
            background-color: #f8f9fa;
            padding: 2px 4px;
            border-radius: 3px;
            font-family: 'Courier New', Courier, monospace;
        }}
        
        pre {{
            background-color: #f8f9fa;
            border: 1px solid #e9ecef;
            border-radius: 5px;
            padding: 15px;
            overflow-x: auto;
            margin: 15px 0;
        }}
        
        pre code {{
            background-color: transparent;
            padding: 0;
        }}
        
        table {{
            border-collapse: collapse;
            width: 100%;
            margin: 15px 0;
        }}
        
        table, th, td {{
            border: 1px solid #ddd;
        }}
        
        th, td {{
            padding: 12px;
            text-align: left;
        }}
        
        th {{
            background-color: #f2f2f2;
            font-weight: bold;
        }}
        
        tr:nth-child(even) {{
            background-color: #f9f9f9;
        }}
        
        blockquote {{
            border-left: 4px solid #3498db;
            margin: 0;
            padding-left: 20px;
            background-color: #f8f9fa;
            padding: 10px 20px;
            margin: 15px 0;
        }}
        
        ul, ol {{
            margin: 15px 0;
        }}
        
        li {{
            margin: 5px 0;
        }}
        
        .toc {{
            background-color: #f8f9fa;
            border: 1px solid #e9ecef;
            border-radius: 5px;
            padding: 20px;
            margin: 20px 0;
        }}
        
        .toc ul {{
            margin: 0;
            padding-left: 20px;
        }}
        
        @media print {{
            body {{
                max-width: none;
                margin: 0;
                padding: 15px;
            }}
            
            h1, h2 {{
                page-break-after: avoid;
            }}
            
            pre, table {{
                page-break-inside: avoid;
            }}
        }}
    </style>
</head>
<body>
{content}
</body>
</html>"""
    
    # Generate output filename
    if output_path is None:
        output_path = md_file_path.rsplit('.', 1)[0] + '.html'
    
    # Write HTML file
    html_content = html_template.format(content=html_body)
    
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(html_content)
    
    print(f"✅ Successfully converted to: {output_path}")
    print(f"📄 File size: {len(html_content):,} characters")
    return True

def main():
    """Main function"""
    # Default input file
    md_file = "BTD_Erlang_Project_Documentation.md"
    
    if len(sys.argv) > 1:
        md_file = sys.argv[1]
    
    print("🚀 BTD-Erlang Documentation Converter")
    print("=" * 50)
    print(f"📖 Converting: {md_file}")
    
    if convert_md_to_html(md_file):
        print("\n✨ Conversion completed successfully!")
        print("\n📋 Next steps to create PDF:")
        print("1. Open the HTML file in Chrome/Firefox")
        print("2. Press Ctrl+P (or Cmd+P on Mac)")
        print("3. Select 'Save as PDF' as destination")
        print("4. Choose 'More settings' > Layout: Portrait")
        print("5. Set margins to 'Minimum' for best formatting")
        print("6. Click 'Save' to generate PDF")
        print("\n🌐 Alternative: Use online HTML to PDF converters:")
        print("   - https://html-pdf-converter.com/")
        print("   - https://www.sejda.com/html-to-pdf")
        print("   - https://smallpdf.com/html-to-pdf")
    else:
        print("❌ Conversion failed!")
        sys.exit(1)

if __name__ == "__main__":
    main()
