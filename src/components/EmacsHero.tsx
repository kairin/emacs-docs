import React from 'react';
import { Button } from './ui/Button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from './ui/Card';

export function EmacsHero() {
  return (
    <div className="container mx-auto px-4 py-16">
      <div className="text-center mb-16">
        <h1 className="text-4xl md:text-6xl font-bold mb-6 bg-gradient-to-r from-purple-400 to-blue-400 bg-clip-text text-transparent">
          Emacs Documentation
        </h1>
        <p className="text-xl text-muted-foreground mb-8 max-w-2xl mx-auto">
          Comprehensive documentation for Emacs, the extensible, customizable, free/libre text editor—and more.
        </p>
        <div className="flex gap-4 justify-center flex-wrap">
          <Button size="lg" asChild>
            <a href="/emacs-docs/getting-started">Get Started</a>
          </Button>
          <Button variant="outline" size="lg" asChild>
            <a href="/emacs-docs/docs">Browse Docs</a>
          </Button>
        </div>
      </div>

      <div className="grid md:grid-cols-3 gap-6">
        <Card>
          <CardHeader>
            <CardTitle>🚀 Getting Started</CardTitle>
            <CardDescription>
              Installation, basic configuration, and first steps with Emacs
            </CardDescription>
          </CardHeader>
          <CardContent>
            <p className="text-sm text-muted-foreground mb-4">
              Learn how to install Emacs, understand the basics, and get up and running quickly.
            </p>
            <Button variant="ghost" size="sm" asChild>
              <a href="/emacs-docs/getting-started">Learn More →</a>
            </Button>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>⚙️ Configuration</CardTitle>
            <CardDescription>
              Customize Emacs to fit your workflow and preferences
            </CardDescription>
          </CardHeader>
          <CardContent>
            <p className="text-sm text-muted-foreground mb-4">
              Explore configuration options, packages, and how to make Emacs your own.
            </p>
            <Button variant="ghost" size="sm" asChild>
              <a href="/emacs-docs/configuration">Learn More →</a>
            </Button>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>🔧 Advanced Topics</CardTitle>
            <CardDescription>
              Deep dive into Elisp, custom packages, and advanced features
            </CardDescription>
          </CardHeader>
          <CardContent>
            <p className="text-sm text-muted-foreground mb-4">
              Master advanced Emacs features, write Elisp, and extend functionality.
            </p>
            <Button variant="ghost" size="sm" asChild>
              <a href="/emacs-docs/advanced">Learn More →</a>
            </Button>
          </CardContent>
        </Card>
      </div>
    </div>
  );
}